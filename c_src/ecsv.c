// Copyright 2016 Altworx. All rights reserved.
#include "ecsv_nif.h"
#include "ecsv_atoms.h"
#include "ecsv_parser.h"
#include "ecsv_writer.h"

static int
load(ErlNifEnv *env, UNUSED(void **priv), UNUSED(ERL_NIF_TERM load_info))
{
    init_ecsv_atoms(env);
    return set_ecsv_parser_type(env);
}

static inline uint64_t clp2l(uint64_t x) { return x>1?(uint64_t)INT64_MIN >> (__builtin_clzl(x-1)-1):x; }

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
    ECSV_PARSER_NIFS,
    ECSV_WRITER_NIFS,
    {"test", 1, test}
};
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv_nif, nif_funcs, &load, NULL, NULL, NULL)
#pragma GCC visibility pop
