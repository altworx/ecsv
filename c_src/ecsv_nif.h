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

#ifdef trace_debug
#   define debug_printf(s, ...) do {fprintf(stderr, s "\r\n", ##__VA_ARGS__);} while(0)
#else
#   define debug_printf(s, ...) while(0) {fprintf(stderr, s "\r\n", ##__VA_ARGS__);}
#endif

static __inline__ void *
erealloc(void *ptr, const size_t size) {
    debug_printf("\tRealloc %zu at %p", size, ptr);
    void *newptr = enif_realloc(ptr, size);
    debug_printf("\tAllocated at %p-%p", newptr, (char *)newptr+size);
    return newptr;
}
