//#define trace_debug
#include "ecsv_nif.h"
#include <csv.h>

enum {
    START_FIELDS = 8,
};

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
static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM eof;
    ERL_NIF_TERM insufficient_memory;
    ERL_NIF_TERM parse_error;
    ERL_NIF_TERM owner_mismatch;
} atoms;

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

    unsigned char *field = enif_make_new_binary(env, len, &line->fields[line->pos++]);
    memcpy(field, s, len);
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
static ErlNifFunc nif_funcs[] =
{
    {"parser_init", 2, parser_init},
    {"parse_",       3, parse}
};
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv, nif_funcs, &load, NULL, NULL, NULL)
#pragma GCC visibility pop
