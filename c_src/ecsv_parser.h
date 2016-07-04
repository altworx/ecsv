// Copyright 2016 Altworx. All rights reserved.
#pragma once

#include "ecsv_nif.h"

#define ECSV_PARSER_NIFS \
    {"parser_init", 1, parser_init}, \
    {"parse",   3, parse}

NIF(parser_init);
NIF(parse);

int set_ecsv_parser_type(ErlNifEnv *env);
