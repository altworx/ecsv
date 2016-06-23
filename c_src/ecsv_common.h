// Copyright 2015 Altworx. All rights reserved.
#pragma once

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define __v(x, c) __ ## x ## _ ## c

#define likely(expr)     (__builtin_expect(!!(expr), 1))
#define unlikely(expr)   (__builtin_expect(!!(expr), 0))

#define unless(expr) if(!(expr))

#define max(a,b) max_(__COUNTER__, a, b)
#define max_(c, ...) \
    max__(__v(x,c), __v(y,c), __VA_ARGS__)
#define max__(x, y, a, b) \
    ({ \
     register const typeof(a) x = (a); \
     register const typeof(b) y = (b); \
     x > y ? x : y; \
     })

#define min(a,b) min_(__COUNTER__, a, b)
#define min_(c, ...) \
    min__(__v(x,c), __v(y,c), __VA_ARGS__)
#define min__(x, y, a, b) \
    ({ \
     register const typeof(a) x = (a); \
     register const typeof(b) y = (b); \
     x < y ? x : y; \
     })

#define swap(a,b) swap_(__COUNTER__, a, b)
#define swap_(c, ...) \
    swap__(__v(tmp, c), __VA_ARGS__)
#define swap__(tmp, a, b) \
    ({ \
        register const typeof(a) tmp = (a); \
        (a) = (b); \
        (b) = tmp; \
     })

#define UNUSED(x) x __attribute__((__unused__))
