// Copyright 2016 Altworx. All rights reserved.

#include "ecsv_atoms.h"

struct atoms atoms;

#define make_atom(X) atoms.X = enif_make_atom(env, #X)
void init_ecsv_atoms(ErlNifEnv *env) {
    for_atoms(make_atom);
}
#undef make_atom
