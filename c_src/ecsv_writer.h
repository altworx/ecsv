// Copyright 2016 Altworx. All rights reserved.
#pragma once

#include "ecsv_nif.h"

#define ECSV_WRITER_NIFS \
    {"write", 1, write}, \
    {"write_lines", 1, write_lines}

NIF(write);
NIF(write_lines);
