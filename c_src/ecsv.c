//#define trace_debug
#include "ecsv_nif.h"
#include <csv.h>

enum {
    START_FIELDS = 8,
    START_BUFFER_SIZE = 64,
    INT_SPACE = 21,         // 20 + 1 for sign
    FLOAT_SPACE = 30,
};

#define for_atoms(M) \
    M(ok); \
    M(error); \
    M(eof); \
    M(insufficient_memory); \
    M(parse_error); \
    M(owner_mismatch); \
    M(strict); \
    M(all_lines); \
    M(strict_finish); \
    M(null); \
    M(delimiter); \
    M(quote); \
    M(bad_option)
typedef struct {
    ERL_NIF_TERM *fields;
    size_t len;
    size_t pos;
} line_t;

typedef struct {
    ErlNifEnv *env;
    ErlNifPid owner;
    line_t current_line;
    ERL_NIF_TERM lines;
    size_t len;
    size_t pos;
    struct csv_parser p;
    int err;
} ecsv_parser_t;

static ErlNifResourceType *ecsv_parser_type = NULL;
#define declare_field(X) ERL_NIF_TERM X
static struct {
    for_atoms(declare_field);
} atoms;

static inline uint64_t clp2l(uint64_t x) { return x>1?(uint64_t)INT64_MIN >> (__builtin_clzl(x-1)-1):x; }

static void field_call_back(void *s, size_t len, void *data)
{
    ecsv_parser_t *parser = (ecsv_parser_t *)data;
    if (parser->err) return;
    ErlNifEnv *env = parser->env;
    line_t *line = &parser->current_line;

    if (line->pos >= line->len) {
        void *old_ptr = line->fields;
        line->len = line->pos < START_FIELDS ? START_FIELDS : line->pos * 2;
        line->fields = erealloc(line->fields, line->len * sizeof(line->fields[0]));
        if (!(line->fields)) {
            enif_free(old_ptr);
            line->len = 0; // just for sure
            parser->err = 1;
            return;
        }
    }

    if (s) {
        unsigned char *field = enif_make_new_binary(env, len, &line->fields[line->pos]);
        memcpy(field, s, len);
    } else {
        line->fields[line->pos] = atoms.null;
    }
    line->pos++;
}


static void line_call_back(UNUSED(int c), void *data)
{
    ecsv_parser_t *parser = (ecsv_parser_t *)data;
    if (parser->err) return;
    ErlNifEnv *env = parser->env;
    line_t *line = &parser->current_line;
    parser->lines = enif_make_list_cell(env,
            enif_make_tuple_from_array(env, line->fields, line->pos),
            parser->lines);
    line->pos = 0;
}

NIF(parser_init)
{
    unsigned int delimiter = CSV_COMMA;
    unsigned int quote = CSV_QUOTE;
    ecsv_parser_t *parser;
    unsigned char options = 0;
    ERL_NIF_TERM term, head, list = argv[0];

    unless (argc == 1) return enif_make_badarg(env);

    while (enif_get_list_cell(env, list, &head, &list)) {
        const ERL_NIF_TERM *elems;
        int arity;
        if (enif_is_identical(atoms.strict, head)) {
            options |= CSV_STRICT;
        } else if (enif_is_identical(atoms.null, head)) {
            options |= CSV_EMPTY_IS_NULL;
        } else if (enif_is_identical(atoms.all_lines, head)) {
            options |= CSV_REPALL_NL;
        } else if (enif_is_identical(atoms.strict_finish, head)) {
            options |= CSV_STRICT_FINI;
        } else unless (
                enif_get_tuple(env, head, &arity, &elems) &&
                arity == 2 &&
                ((enif_is_identical(atoms.delimiter, elems[0]) &&
                  enif_get_uint(env, elems[1], &delimiter) &&
                  delimiter < 0x100) ||
                 (enif_is_identical(atoms.quote, elems[0]) &&
                  enif_get_uint(env, elems[1], &quote) &&
                  quote < 0x100))
                ) {
            return enif_raise_exception(env,
                    enif_make_tuple2(env, atoms.bad_option, head));
        }
    }

    unless (enif_is_empty_list(env, list)) return enif_make_badarg(env);

    unless ((parser = enif_alloc_resource(ecsv_parser_type, sizeof(*parser)))) {
        term = enif_raise_exception(env, atoms.insufficient_memory);
        goto alloc_error;
    };
    *parser = (ecsv_parser_t){0};

    if (csv_init(&parser->p, options)) {
        term = enif_raise_exception(env, enif_make_atom(env, "libcsv_init"));
        goto init_error;
    }

    csv_set_delim(&parser->p, delimiter);
    csv_set_quote(&parser->p, quote);
    csv_set_realloc_func(&parser->p, erealloc);
    csv_set_free_func(&parser->p, enif_free);

    enif_self(env, &parser->owner);
    parser->env = enif_alloc_env();

    term = enif_make_resource(env, parser);
init_error:
    enif_release_resource(parser);
alloc_error:
    return term;
}

static void
release_ecsv_parser(UNUSED(ErlNifEnv * env), void *obj)
{
    //enif_fprintf(stderr, "Releasing parser resource\n");
    ecsv_parser_t *parser = (ecsv_parser_t *)obj;
    enif_free_env(parser->env);
    if (parser->current_line.fields) enif_free(parser->current_line.fields);
    csv_free(&parser->p);
    //enif_fprintf(stderr, "Released parser resource\n");
}

static int
load(ErlNifEnv *env, UNUSED(void **priv), UNUSED(ERL_NIF_TERM load_info))
{
#define make_atom(X) atoms.X = enif_make_atom(env, #X)
    for_atoms(make_atom);
    ecsv_parser_type = enif_open_resource_type(
            env, NULL, "ecsv_parser", release_ecsv_parser,
            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return !ecsv_parser_type;
}

static ErlNifEnv *
copy_current_line(ErlNifEnv *env, ecsv_parser_t *parser)
{
    ErlNifEnv *old = parser->env;
    line_t *line = &parser->current_line;

    parser->env = env;
    for (size_t i=0; i < line->pos; i++) {
        line->fields[i] = enif_make_copy(env, line->fields[i]);
    }

    return old;
}

static ERL_NIF_TERM
return_so_far(ErlNifEnv *env, ecsv_parser_t *parser)
{
    if (parser->err)
        return enif_raise_exception(env, atoms.insufficient_memory);

    if (env != parser->env) { // it should never happen
        return enif_raise_exception(env, enif_make_atom(env, "env_mismatch"));
    }

    copy_current_line(enif_alloc_env(), parser);

    return parser->lines;
}

NIF(parse)
{
    ErlNifBinary bin_raw;
    ecsv_parser_t *parser;
    int eof = 0;

    unless (argc == 3 &&
            (
             (eof = enif_is_identical(argv[0], atoms.eof)) ||
             enif_inspect_binary(env, argv[0], &bin_raw)
            ) &&
            enif_get_resource(env, argv[1], ecsv_parser_type, (void **)&parser)
           ) {
        return enif_make_badarg(env);
    }

    {
        ErlNifPid self_pid;
        ERL_NIF_TERM self = enif_make_pid(env, enif_self(env, &self_pid));
        ERL_NIF_TERM owner = enif_make_pid(env, &parser->owner);
        unless (enif_is_identical(self, owner)) {
            return enif_raise_exception(env,
                    enif_make_tuple2(env, atoms.owner_mismatch, owner)
                    );
        }
    }

    ErlNifTime start = enif_monotonic_time(ERL_NIF_USEC);

    enif_free_env(copy_current_line(env, parser));
    parser->lines = argv[2];

    if (eof) {
        csv_fini(&parser->p, field_call_back, line_call_back, parser);
        csv_free(&parser->p);
    } else {
        csv_parse( &parser->p, bin_raw.data, bin_raw.size, field_call_back, line_call_back, parser);
    }
    ERL_NIF_TERM result = return_so_far(env, parser);

    {
        ErlNifTime diff = enif_monotonic_time(ERL_NIF_USEC) - start;
        int percent = diff / 10;
        if (percent > 100) percent = 100;
        enif_consume_timeslice(env, percent);
    }

    return  csv_error(&parser->p)
        ? enif_make_tuple3(env,
                atoms.error,
                result,
                enif_make_tuple2(env,
                    atoms.parse_error,
                    enif_make_string(env, csv_strerror(csv_error(&parser->p)),
                        ERL_NIF_LATIN1)
                    )
                )
        : enif_make_tuple3(env, atoms.ok, result, argv[1]);
}

static inline int resize_binary(ErlNifBinary *bin, size_t size) {
    size_t s = size + 1; // add space for ',' or '\n'
    if (s < START_BUFFER_SIZE) s = START_BUFFER_SIZE;
    if (s > bin->size) {
        s = clp2l(s);
        return bin->size ? enif_realloc_binary(bin, s) : enif_alloc_binary(s, bin);
    }
    return 1;
}

NIF(write)
{
    ErlNifBinary buf = {0};
    ERL_NIF_TERM result, head, list = argv[0];
    size_t p = 0;

    while (enif_get_list_cell(env, list, &head, &list)) {
        ErlNifBinary bin;
        ErlNifSInt64 i;
        double d;
        char atom_buff[256];
        unsigned atom_len;
        size_t s = 0;
        if (enif_get_int64(env, head, &i)) {
            unless (resize_binary(&buf, p + INT_SPACE)) {
                result = enif_raise_exception(env, atoms.insufficient_memory);
                goto error;
            }
            s = snprintf((char *)buf.data + p, buf.size - p, "%li", i);
        } else if (enif_get_double(env, head, &d)) {
            unless (resize_binary(&buf, p + FLOAT_SPACE)) {
                result = enif_raise_exception(env, atoms.insufficient_memory);
                goto error;
            }
            s = snprintf((char *)buf.data + p, buf.size - p, "%.16lg", d);
        } else if ((atom_len = enif_get_atom(env, head, atom_buff, 256, ERL_NIF_LATIN1))) {
            unless (resize_binary(&buf, p + atom_len * 2 + 2)) {
                result = enif_raise_exception(env, atoms.insufficient_memory);
                goto error;
            }
            s = csv_write((char *)buf.data + p, buf.size - p, atom_buff, atom_len-1);
        } else if (enif_inspect_binary(env, head, &bin) ||
                enif_inspect_iolist_as_binary(env, head, &bin)) {
            unless (resize_binary(&buf, p + bin.size * 2 + 2)) {
                result = enif_raise_exception(env, atoms.insufficient_memory);
                goto error;
            }
            s = csv_write((char *)buf.data + p, buf.size - p, bin.data, bin.size);
        } else {
            result = enif_make_badarg(env);
            goto error;
        }
        p += s;
        buf.data[p++] = CSV_COMMA;
    }
    if (p) buf.data[p-1] = CSV_LF;

    unless (enif_is_empty_list(env, list)) {
        result = enif_make_badarg(env);
        goto error;
    }

    enif_realloc_binary(&buf, p);
    result = enif_make_binary(env, &buf);
    return result;

error:
    if (buf.size) enif_release_binary(&buf);
    return result;
}

NIF(test)
{
    ErlNifUInt64 v;

    unless (argc == 1 && enif_get_uint64(env, argv[0], &v))
        return enif_make_badarg(env);

    return enif_make_uint64(env, clp2l(v));
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
static ErlNifFunc nif_funcs[] =
{
    {"test", 1, test},
    {"write", 1, write},
    {"parser_init", 1, parser_init},
    {"parse_nif",   3, parse}
};
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv, nif_funcs, &load, NULL, NULL, NULL)
#pragma GCC visibility pop
