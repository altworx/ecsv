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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
static ErlNifFunc nif_funcs[] =
{
    ECSV_PARSER_NIFS,
    ECSV_WRITER_NIFS,
};
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
ERL_NIF_INIT(ecsv_nif, nif_funcs, &load, NULL, NULL, NULL)
#pragma GCC visibility pop
