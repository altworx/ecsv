// Copyright 2016 Altworx. All rights reserved.
#pragma once

#include <erl_nif.h>
#include "ecsv_common.h"

#define NIF(name) ERL_NIF_TERM name( \
        UNUSED(ErlNifEnv* env), \
        UNUSED(int argc), \
        UNUSED(const ERL_NIF_TERM argv[]))

#define PAR(test) unless(test) return enif_make_badarg(env)

static __inline__ void *
ealloc(const size_t size) __attribute__ ((malloc, alloc_size (1)));
static __inline__ void *
ealloc(const size_t size) { return enif_alloc(size); }

static __inline__ void *
erealloc(void *ptr, const size_t size) __attribute__ ((alloc_size (2)));
static __inline__ void *
erealloc(void *ptr, const size_t size) {
    enif_fprintf(stderr, "\tRealloc %u at %p\n", size, ptr);
    void *newptr = enif_realloc(ptr, size);
    enif_fprintf(stderr, "\tAllocated at %p-%p\n", newptr, (char *)newptr+size);
    return newptr;
}
