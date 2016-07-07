// Copyright 2016 Altworx. All rights reserved.
#pragma once

#include <erl_nif.h>

#define TIME_UNIT ERL_NIF_USEC
#define UNITS_PER_PERCENT 10

static __inline__ int consume_time(ErlNifEnv *env, ErlNifTime start) {
  ErlNifTime consumed = ( enif_monotonic_time(TIME_UNIT) - start) / UNITS_PER_PERCENT;
  return (consumed > 0) ? enif_consume_timeslice(env, consumed > 100 ? 100 : consumed) : 0;
}

#define T_START ErlNifTime t_start = enif_monotonic_time(TIME_UNIT)
#define T_STOP consume_time(env, t_start)
