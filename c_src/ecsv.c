//#define trace_debug
#include "ecsv_nif.h"
#include <csv.h>

enum {
    START_FIELDS = 8,
    START_BUFFER_SIZE = 8,
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
    M(bad_option); \
    M(badarg)
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

static inline unsigned chars4i(int64_t x) {
    return x < 0
        ? (x > -1000000L
                ? 7
                : (x > -100000000000000L
                    ? 15
                    : 20))
        : (x < 10000000L
                ? 7
                : (x < 1000000000000000L
                    ? 15
                    : 19));
}

typedef struct {
    ErlNifBinary bin;
    size_t pos;
} bin_buff_t;

typedef struct {
    bin_buff_t current;
    ERL_NIF_TERM *terms;
    size_t pos;
    size_t len;
    ErlNifEnv *env;
} writer_st_t;

static void init_writer(ErlNifEnv *env, writer_st_t *writer) {
    *writer = (writer_st_t){0};
    writer->env = env;
}

static void release_writer_st(writer_st_t *writer) {
    if (writer->current.bin.size) enif_release_binary(&writer->current.bin);
    if (writer->terms) enif_free(writer->terms);
    *writer = (writer_st_t){0};
}

static int add_term(writer_st_t *writer, ERL_NIF_TERM term) {
    if unlikely(writer->pos >= writer->len) {
        void *old_ptr = writer->terms;
        writer->len = writer->pos < START_FIELDS ? START_FIELDS : writer->pos * 2;
        writer->terms = erealloc(writer->terms, writer->len * sizeof(writer->terms[0]));
        unless (writer->terms) {
            enif_free(old_ptr);
            writer->len = 0;
            return 0;
        }
    }
    writer->terms[writer->pos++] = term;
    return 1;
}

static int finish_current_buffer(writer_st_t *writer) {
    if (writer->current.pos) {
        unless (writer->current.pos == writer->current.bin.size ||
                (writer->current.pos < writer->current.bin.size &&
                 enif_realloc_binary(&writer->current.bin, writer->current.pos))) {
            enif_release_binary(&writer->current.bin);
            writer->current = (bin_buff_t){0};
            return 0;
        };
        ERL_NIF_TERM term = enif_make_binary(writer->env, &writer->current.bin);
        int success = add_term(writer, term);
        writer->current = (bin_buff_t){0};
        return success;
    }
    return 1;
}

static ERL_NIF_TERM return_result(writer_st_t *writer) {
    unless (finish_current_buffer(writer)) {
        release_writer_st(writer);
        return enif_raise_exception(writer->env, atoms.insufficient_memory);
    }
    ERL_NIF_TERM result = writer->pos == 1
        ? writer->terms[0]
        : enif_make_list_from_array(writer->env, writer->terms, writer->pos);
    release_writer_st(writer);
    return result;
}

static int resize_buffer(bin_buff_t *buf, size_t inc) {
    size_t size = buf->pos + inc;
    size = size < START_BUFFER_SIZE ? START_BUFFER_SIZE : clp2l(size);
    int success = buf->bin.size
        ? enif_realloc_binary(&buf->bin, size)
        : enif_alloc_binary(size, &buf->bin);
    if (success) {
        return 1;
    } else {
        if (buf->bin.size) enif_release_binary(&buf->bin);
        *buf = (bin_buff_t){0};
        return 0;
    }
}

static inline int grow_current_buffer(writer_st_t *writer, size_t inc) {
    return writer->current.pos + inc < writer->current.bin.size ||
        resize_buffer(&writer->current, inc);
}

static inline char *current_buffer_position(writer_st_t *writer) {
    return (char *)writer->current.bin.data + writer->current.pos;
}

static inline size_t free_space(writer_st_t *writer) {
    return writer->current.bin.size - writer->current.pos;
}

static inline void seek_current_buffer(writer_st_t *writer, size_t inc) {
    if (grow_current_buffer(writer, inc))
        writer->current.pos += inc;
}

static inline void putc_current_buffer_pos(writer_st_t *writer, char ch) {
    if (grow_current_buffer(writer, 1))
        writer->current.bin.data[writer->current.pos++] = ch;
}

#define grow_buffer(writer, size) \
    unless (grow_current_buffer((writer), (size))) { \
        enif_raise_exception(env, atoms.insufficient_memory); \
        return 0; \
    }

static int try_write_int(writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
    ErlNifSInt64 i;
    if (enif_get_int64(env, term, &i)) {
        grow_buffer(writer, chars4i(i)+1);
        seek_current_buffer(writer,
                snprintf(current_buffer_position(writer),
                    free_space(writer), "%li", i));
        return 1;
    } else {
        return 0;
    }
}

static int try_write_double(writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
    double d;
    if (enif_get_double(env, term, &d)) {
        grow_buffer(writer, FLOAT_SPACE);
        seek_current_buffer(writer,
                snprintf(current_buffer_position(writer),
                    free_space(writer), "%.16lg", d));
        return 1;
    } else {
        return 0;
    }
}

static int must_quote(const unsigned char *data, const size_t len)
{
    if (data[0] == CSV_SPACE || data[0] == CSV_TAB || data[len-1] == CSV_SPACE || data[len-1] == CSV_TAB)
        return 1;
    for (size_t i = 0; i < len; i++) {
        switch (data[i]) {
            case CSV_QUOTE :
            case CSV_COMMA :
            case CSV_CR :
            case CSV_LF :
                return 1;
        }
    }
    return 0;
}

static int try_write_atom(writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
#define ATOM_BUFF_LENGTH 256
    char atom_buff[ATOM_BUFF_LENGTH];
    unsigned atom_len;
    if ((atom_len = enif_get_atom(env, term, atom_buff,
                    ATOM_BUFF_LENGTH, ERL_NIF_LATIN1))) {
        if (must_quote((unsigned char *)atom_buff, atom_len-1)) {
            grow_buffer(writer, atom_len * 2 + 2);
            seek_current_buffer(writer,
                    csv_write(current_buffer_position(writer),
                        free_space(writer), atom_buff, atom_len-1));
        } else {
            grow_buffer(writer, atom_len);
            enif_get_atom(env, term, current_buffer_position(writer),
                    free_space(writer), ERL_NIF_LATIN1);
            seek_current_buffer(writer, atom_len-1);
        }
        return 1;
    } else {
        return 0;
    }
}

static int try_write_binary_or_iolist (writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
    ErlNifBinary bin;
    if (enif_inspect_binary(env, term, &bin) ||
            enif_inspect_iolist_as_binary(env, term, &bin)) {
        if (must_quote(bin.data, bin.size)) {
            grow_buffer(writer, bin.size * 2 + 3);
            seek_current_buffer(writer,
                    csv_write(current_buffer_position(writer), free_space(writer),
                        bin.data, bin.size));
        } else {
            unless (finish_current_buffer(writer) && add_term(writer, term)) {
                enif_raise_exception(env, atoms.insufficient_memory);
                return 0;
            }
        }
        return 1;
    } else {
        return 0;
    }
}

static int write_line(writer_st_t *writer, ERL_NIF_TERM list) {
    ErlNifEnv *env = writer->env;
    ERL_NIF_TERM head;
    const ERL_NIF_TERM comma = enif_make_uint(env, CSV_COMMA);
    const ERL_NIF_TERM lf    = enif_make_uint(env, CSV_LF);

    while (enif_get_list_cell(env, list, &head, &list)) {
        unless (try_write_int(writer, head) ||
                try_write_double(writer, head) ||
                try_write_atom(writer, head) ||
                try_write_binary_or_iolist(writer, head)) {
            unless (enif_has_pending_exception(env, NULL)) {
                enif_raise_exception(env, enif_make_tuple2(env, atoms.badarg, head));
            }
            return 0;
        }
        if (writer->current.bin.size) {
            putc_current_buffer_pos(writer, CSV_COMMA);
        } else unless (add_term(writer, comma)) {
            enif_raise_exception(env, atoms.insufficient_memory);
            return 0;
        }
    }
    if (writer->current.pos) {
        seek_current_buffer(writer, -1);
        putc_current_buffer_pos(writer, CSV_LF);
    } else if (writer->pos) {
        writer->terms[writer->pos-1] = lf;
    }

    unless (enif_is_empty_list(env, list)) {
        enif_raise_exception(env, enif_make_tuple2(env, atoms.badarg, list));
        return 0;
    }
    return 1;
}

NIF(write)
{
    writer_st_t writer;
    init_writer(env, &writer);
    ERL_NIF_TERM result;

    if (write_line(&writer, argv[0])) {
        result = return_result(&writer);
    } else {
        release_writer_st(&writer);
        enif_has_pending_exception(env, &result);
    }

    return result;
}

NIF(write_lines)
{
    writer_st_t writer;
    init_writer(env, &writer);
    ERL_NIF_TERM result, head, list = argv[0];


    while (enif_get_list_cell(env, list, &head, &list) && write_line(&writer, head)) {
    };
    if (enif_is_empty_list(env, list)) {
        return return_result(&writer);
    } else if (enif_has_pending_exception(env, &result)) {
    } else {
        result = enif_raise_exception(env, enif_make_tuple2(env, atoms.badarg, list));
    }
    release_writer_st(&writer);
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
    {"write_lines", 1, write_lines},
    {"parser_init", 1, parser_init},
    {"parse_nif",   3, parse}
};
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv, nif_funcs, &load, NULL, NULL, NULL)
#pragma GCC visibility pop
