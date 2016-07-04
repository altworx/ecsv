// Copyright 2016 Altworx. All rights reserved.
#pragma once

#include <erl_nif.h>

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

#define declare_field(X) ERL_NIF_TERM X
struct atoms {
    for_atoms(declare_field);
};
#undef declare_field

extern struct atoms atoms;

void init_ecsv_atoms(ErlNifEnv *env);
