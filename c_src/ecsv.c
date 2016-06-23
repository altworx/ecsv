#include "ecsv_nif.h"
#include <csv.h>

enum {
    CHUNK_SIZE = 10*1024,
};

typedef struct {
    long unsigned fields;
    long unsigned rows;
    // TODO: add our data
    ErlNifPid owner;
    struct csv_parser p;
} ecsv_parser_t;

static ErlNifResourceType *ecsv_parser_type = NULL;

void cb1 (void *s, size_t len, void *data)
{
    ((ecsv_parser_t *)data)->fields++;
}


void cb2 (int c, void *data)
{
    ((ecsv_parser_t *)data)->rows++;
}

NIF(parser_init)
{
    unsigned int delimiter;
    unsigned int quote;
    ecsv_parser_t *parser;
    unsigned char options = 0;
    ERL_NIF_TERM term;

    unless (argc == 2 &&
            enif_get_uint(env, argv[0], &delimiter) &&
            enif_get_uint(env, argv[1], &quote)) {
        return enif_make_badarg(env);
    };

    unless ((parser = enif_alloc_resource(ecsv_parser_type, sizeof(ecsv_parser_t)))) {
        term = enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
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

    term = enif_make_resource(env, parser);
init_error:
    enif_release_resource(parser);
alloc_error:
    return term;
}

static void
release_ecsv_parser(UNUSED(ErlNifEnv* env), void* obj) {
    ecsv_parser_t *parser = (ecsv_parser_t *) obj;
    csv_free(&parser->p);
//    enif_fprintf(stderr, "Released parser resource");
}

static int
load(ErlNifEnv* env, UNUSED(void** priv), UNUSED(ERL_NIF_TERM load_info))
{
    ecsv_parser_type = enif_open_resource_type(
            env, NULL, "ecsv_parser", release_ecsv_parser,
            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return !ecsv_parser_type;
}

NIF(parse)
{
    ErlNifBinary bin_raw;
    ecsv_parser_t *parser;
    size_t offset;

    {
        ErlNifUInt64 _offset;
        unless (argc == 3 &&
                enif_inspect_binary(env, argv[0], &bin_raw) &&
                enif_get_resource(env, argv[1], ecsv_parser_type, (void **)&parser) &&
                enif_get_uint64(env, argv[2], &_offset) &&
                _offset <= bin_raw.size
               ) {
            return enif_make_badarg(env);
        }
        offset = _offset;
    }

    {
        ErlNifPid self_pid;
        ERL_NIF_TERM self  = enif_make_pid(env, enif_self(env, &self_pid));
        ERL_NIF_TERM owner = enif_make_pid(env, &parser->owner);
        unless (enif_is_identical(self, owner)) {
            return enif_raise_exception(env,
                    enif_make_tuple2(env,
                        enif_make_atom(env, "owner_mismatch"),
                        owner)
                    );
        }
    }

    while (offset < bin_raw.size) {
        size_t size = bin_raw.size - offset;
        ErlNifTime start = enif_monotonic_time(ERL_NIF_USEC);
        if (size > CHUNK_SIZE) size = CHUNK_SIZE;
        if (csv_parse(&parser->p, bin_raw.data+offset, size, cb1, cb2, parser) != size) {
            return enif_raise_exception(env,
                    enif_make_tuple2(env,
                        enif_make_atom(env, "parse_error"),
                        enif_make_string(env, csv_strerror(csv_error(&parser->p)), ERL_NIF_LATIN1)
                        )
                    );
        };
        ErlNifTime diff = enif_monotonic_time(ERL_NIF_USEC) - start;
        offset += size;
        int percent = diff / 10;
        if (percent > 100) percent = 100;
        enif_fprintf(stderr, "Consume %i%% (%.2f%%)\n", percent, (float)offset / bin_raw.size * 100);
        if (enif_consume_timeslice(env, percent) && offset < bin_raw.size) {
            ERL_NIF_TERM newargv[3] = {argv[0], argv[1], enif_make_uint64(env, (ErlNifUInt64)offset)};
            return enif_schedule_nif(env, "parse_continue", 0, parse, 3, newargv);
        }
    }

    return enif_make_tuple3(env,
            enif_make_atom(env, "ok"),
            enif_make_ulong(env, parser->fields),
            enif_make_ulong(env, parser->rows)
            );
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
static ErlNifFunc nif_funcs[] =
  {
    {"parser_init", 2, parser_init},
    {"parse", 3, parse}
  };
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv, nif_funcs, &load, NULL, NULL, NULL)
#pragma GCC visibility pop
