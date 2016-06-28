#include "ecsv_nif.h"
#include <csv.h>

//#define trace_debug

#ifdef trace_debug
#define debug_printf(s, ...) do {enif_fprintf(stderr, s, __VA_ARGS__);} while(0)
#else
#define debug_printf(s, ...) while(0) {enif_fprintf(stderr, s, __VA_ARGS__);}
#endif

enum {
    CHUNK_SIZE = 10 * 1024,
    START_FIELDS = 8,
    START_LINES = 8,
    START_LINE_SIZE = 64,
};

typedef struct {
    size_t offset;
    size_t len;
} field_t;

typedef struct {
    ErlNifBinary bin;
    field_t *fields;
    size_t len;
    size_t pos;
} line_t;

typedef struct {
    ERL_NIF_TERM *terms;
    size_t len;
} line_tmp_t;

typedef struct {
    ErlNifEnv *env;
    line_t current_line;
    line_tmp_t line_tmp;
    ERL_NIF_TERM *lines;
    size_t len;
    size_t pos;
    ErlNifPid owner;
    struct csv_parser p;
    int err;
} ecsv_parser_t;

static ErlNifResourceType *ecsv_parser_type = NULL;
static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM eof;
    ERL_NIF_TERM insufficient_memory;
    ERL_NIF_TERM parse_error;
    ERL_NIF_TERM owner_mismatch;
} atoms;

static size_t new_offset(line_t *l) {
    size_t line_pos = l->pos;
    if (line_pos) {
        field_t *last_field = l->fields + line_pos - 1;
        return last_field->offset + last_field->len;
    } else {
        return 0;
    }
}

void field_call_back(void *s, size_t len, void *data) {
    ecsv_parser_t *parser = (ecsv_parser_t *) data;
    if (parser->err) return;
    line_t *line = &parser->current_line;
    size_t offset = new_offset(line);

    
    // check line->bin.size and allocate
    debug_printf("line->bin.size %u, offset: %u, len: %u\n", line->bin.size, offset, len);
    if (line->bin.size < offset + len) {
        debug_printf("\tReallocating line->bin\n", 0);
        if (!enif_realloc_binary(&line->bin, (offset + len) * 2)) {
            parser->err = 1;
            return;
        }
        debug_printf("Line binary %u at %p: %s\n", line->bin.size, line->bin.data, line->bin.data);
    }

    // check fields size
    debug_printf("line->len: %u, line->pos: %u\n", line->len, line->pos);
    if (line->pos >= line->len) {
        debug_printf("Realocate line->fields\n", parser->len, parser->pos);
        void *old_ptr = line->fields;
        line->len = likely(line->len) ? line->len * 2 : START_FIELDS;
        line->fields = erealloc(line->fields, line->len * sizeof(line->fields[0]));
        if (!(line->fields)) {
            enif_free(old_ptr);
            line->len = 0; // just for sure
            parser->err = 1;
            return;
        }
    }

    field_t *field = line->fields + line->pos;
    field->offset = offset;
    field->len = len;
    memcpy(line->bin.data + offset, s, len);
    line->pos++;
    debug_printf("Write field %s at start(%p): %p-%p  ===> %s  (len: %u, offset: %u)\n", s, line->bin.data,
                 line->bin.data + offset, line->bin.data + offset + len, line->bin.data, len, offset);
}


void line_call_back(UNUSED(int c), void *data) {
    ecsv_parser_t *parser = (ecsv_parser_t *) data;
    if (parser->err) return;
    line_t *line = &parser->current_line;

    // check lines size
    debug_printf("parser->len %u, parser->pos: %u\n", parser->len, parser->pos);
    if (parser->pos >= parser->len) {
        debug_printf("Realocate parser->lines\n", parser->len, parser->pos);
        void *old_ptr = parser->lines;
        parser->len = likely(parser->len) ? parser->len * 2 : START_LINES;
        parser->lines = erealloc(parser->lines, parser->len * sizeof(parser->lines[0]));
        if (!(parser->lines)) {
            enif_free(old_ptr);
            parser->len = 0; // just for sure
            parser->err = 1;
            return;
        }
    }

    // check size for line term
    line_tmp_t *line_tmp = &parser->line_tmp;
    debug_printf("line->pos %u, line_tmp->len: %u\n", line->pos, line_tmp->len);
    if (line->pos > line_tmp->len) {
        void *old_ptr = line_tmp->terms;
        line_tmp->len = line->pos * 2;
        line_tmp->terms = erealloc(line_tmp->terms, line_tmp->len * sizeof(line_tmp->terms[0]));
        if (!(line_tmp->terms)) {
            enif_free(old_ptr);
            line_tmp->len = 0; // just for sure
            parser->err = 1;
            return;
        }
    }

    ErlNifEnv *env = parser->env;
    if (line->pos) { // nonempty line
        field_t *last_field = line->fields + line->pos - 1;
        size_t line_len = last_field->offset + last_field->len;
        enif_realloc_binary(&line->bin, line_len);
        ERL_NIF_TERM line_term = enif_make_binary(env, &line->bin);
        for (size_t i = 0; i < line->pos; i++) {
            field_t *field = &line->fields[i];
            debug_printf("Writing tmp term %p: %s (%u - %u)\n", line_tmp->terms[i], line->bin.data, field->offset,
                         field->len);
            line_tmp->terms[i] = enif_make_sub_binary(env, line_term, field->offset, field->len);
        }
        parser->lines[parser->pos] = enif_make_tuple_from_array(env, line_tmp->terms, line->pos);
        parser->pos++;
        line->pos = 0;
    }
}

NIF(parser_init) {
    unsigned int delimiter;
    unsigned int quote;
    ecsv_parser_t *parser;
    unsigned char options = CSV_APPEND_NULL;
    ERL_NIF_TERM term;

    unless (argc == 2 &&
            enif_get_uint(env, argv[0], &delimiter) &&
            enif_get_uint(env, argv[1], &quote)) {
        return enif_make_badarg(env);
    };

    unless ((parser = enif_alloc_resource(ecsv_parser_type, sizeof(ecsv_parser_t)))) {
        term = enif_raise_exception(env, atoms.insufficient_memory);
        goto alloc_error;
    };
    *parser = (ecsv_parser_t) {0};

    if (csv_init(&parser->p, options)) {
        term = enif_raise_exception(env, enif_make_atom(env, "libcsv_init"));
        goto init_error;
    }

    csv_set_delim(&parser->p, delimiter);
    csv_set_quote(&parser->p, quote);
    csv_set_realloc_func(&parser->p, erealloc);
    csv_set_free_func(&parser->p, enif_free);

    enif_self(env, &parser->owner);

    term = enif_make_resource(env, parser);
init_error:
    enif_release_resource(parser);
alloc_error:
    return term;
}

static void
release_ecsv_parser(UNUSED(ErlNifEnv * env), void *obj) {
    enif_fprintf(stderr, "Releasing parser resource\n");
    ecsv_parser_t *parser = (ecsv_parser_t *) obj;
    if (parser->current_line.bin.size) enif_release_binary(&parser->current_line.bin);
    if (parser->current_line.fields) enif_free(parser->current_line.fields);
    if (parser->line_tmp.terms) enif_free(parser->line_tmp.terms);
    if (parser->lines) enif_free(parser->lines);
    csv_free(&parser->p);
    enif_fprintf(stderr, "Released parser resource\n");
}

static int
load(ErlNifEnv *env, UNUSED(void **priv), UNUSED(ERL_NIF_TERM
                                                         load_info)) {
    atoms.ok = enif_make_atom(env, "ok");
    atoms.error = enif_make_atom(env, "error");
    atoms.eof = enif_make_atom(env, "eof");
    atoms.insufficient_memory = enif_make_atom(env, "insufficient_memory");
    atoms.parse_error = enif_make_atom(env, "parse_error");
    atoms.owner_mismatch = enif_make_atom(env, "owner_mismatch");
    ecsv_parser_type = enif_open_resource_type(
            env, NULL, "ecsv_parser", release_ecsv_parser,
            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return !ecsv_parser_type;
}

static ERL_NIF_TERM return_so_far(ErlNifEnv *env, ecsv_parser_t *parser) {
    if (parser->err)
        return enif_raise_exception(env, atoms.insufficient_memory);

    ERL_NIF_TERM result = enif_make_list_from_array(env, parser->lines, parser->pos);
    parser->pos = 0;
    return result;
}

NIF(parse) {
    ErlNifBinary bin_raw;
    ecsv_parser_t *parser;
    size_t offset;
    int eof = 0;

    {
        ErlNifUInt64 _offset;
        unless (argc == 3 &&
                (
                        (eof = enif_is_identical(argv[0], atoms.eof)) ||
                        enif_inspect_binary(env, argv[0], &bin_raw)
                ) &&
                enif_get_resource(env, argv[1], ecsv_parser_type, (void **) &parser) &&
                enif_get_uint64(env, argv[2], &_offset) &&
                _offset <= bin_raw.size
        ) {
            return enif_make_badarg(env);
        }
        offset = _offset;
    }

    {
        ErlNifPid self_pid;
        ERL_NIF_TERM self = enif_make_pid(env, enif_self(env, &self_pid));
        ERL_NIF_TERM owner = enif_make_pid(env, &parser->owner);
        unless (enif_is_identical(self, owner)) {
            return enif_raise_exception(env,
                                        enif_make_tuple2(env,
                                                         atoms.owner_mismatch,
                                                         owner)
            );
        }
    }

    // Terms can be made only during one call and always returned from this
    // call
    parser->env = env;

    if (eof) {
        ErlNifTime start = enif_monotonic_time(ERL_NIF_USEC);
        csv_fini(&parser->p, field_call_back, line_call_back, parser);
        if (csv_error(&parser->p)) {
            return enif_make_tuple3(env,
                                    atoms.error,
                                    return_so_far(env, parser),
                                    enif_make_tuple2(env,
                                                     atoms.parse_error,
                                                     enif_make_string(env, csv_strerror(csv_error(&parser->p)),
                                                                      ERL_NIF_LATIN1)
                                    )
            );
        }
        ErlNifTime diff = enif_monotonic_time(ERL_NIF_USEC) - start;
        int percent = diff / 10;
        if (percent > 100) percent = 100;
        enif_consume_timeslice(env, percent);
    }
    else {
        while (offset < bin_raw.size) {
            size_t size = bin_raw.size - offset;
            ErlNifTime start = enif_monotonic_time(ERL_NIF_USEC);
            if (size > CHUNK_SIZE) size = CHUNK_SIZE;
            if (csv_parse(&parser->p, bin_raw.data + offset, size, field_call_back, line_call_back, parser) != size) {
                return enif_make_tuple3(env,
                                        atoms.error,
                                        return_so_far(env, parser),
                                        enif_make_tuple2(env,
                                                         atoms.parse_error,
                                                         enif_make_string(env, csv_strerror(csv_error(&parser->p)),
                                                                          ERL_NIF_LATIN1)
                                        )
                );
            };
            ErlNifTime diff = enif_monotonic_time(ERL_NIF_USEC) - start;
            offset += size;
            int percent = diff / 10;
            if (percent > 100) percent = 100;
            enif_fprintf(stderr, "Consume %i%% (%.2f%%)\n", percent, (float) offset / bin_raw.size * 100);
            if (enif_consume_timeslice(env, percent) && offset < bin_raw.size) {
                ERL_NIF_TERM newargv[3] = {argv[0], argv[1], enif_make_uint64(env, (ErlNifUInt64) offset)};
                return enif_schedule_nif(env, "parse", 0, parse, 3, newargv);
            }
        }
    }

    enif_fprintf(stderr, "Generate  result\n");
    ERL_NIF_TERM result = return_so_far(env, parser);
    enif_fprintf(stderr, "Result finished\n");
    return enif_make_tuple3(env,
                            atoms.ok,
                            result,
                            argv[1]
    );
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
static ErlNifFunc nif_funcs[] =
        {
                {"parser_init", 2, parser_init},
                {"parse",       3, parse}
        };
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv, nif_funcs, &load, NULL, NULL, NULL
)
#pragma GCC visibility pop
