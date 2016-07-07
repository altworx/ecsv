// Copyright 2016 Altworx. All rights reserved.

#include "ecsv_nif.h"
#include "ecsv_atoms.h"
#include "ecsv_time.h"
#include <csv.h>

enum {
    START_FIELDS = 8,
    START_BUFFER_SIZE = 8,
    FLOAT_SPACE = 30,
};

static inline uint64_t clp2l(uint64_t x) { return x>1?(uint64_t)INT64_MIN >> (__builtin_clzl(x-1)-1):x; }

static inline unsigned chars4i(int64_t x) {
    return x < 0
        ? (x > -1000000L
                ? 7
                : (x > -100000000000000L
                    ? 15
                    : 20))
        : (x < 10000000L
                ? 7
                : (x < 1000000000000000L
                    ? 15
                    : 19));
}

typedef struct {
    ErlNifBinary bin;
    size_t pos;
} bin_buff_t;

typedef struct {
    bin_buff_t current;
    ERL_NIF_TERM *terms;
    size_t pos;
    size_t len;
    ErlNifEnv *env;
} writer_st_t;

static void init_writer(ErlNifEnv *env, writer_st_t *writer) {
    *writer = (writer_st_t){0};
    writer->env = env;
}

static void release_writer_st(writer_st_t *writer) {
    if (writer->current.bin.size) enif_release_binary(&writer->current.bin);
    if (writer->terms) enif_free(writer->terms);
    *writer = (writer_st_t){0};
}

static int add_term(writer_st_t *writer, ERL_NIF_TERM term) {
    if unlikely(writer->pos >= writer->len) {
        void *old_ptr = writer->terms;
        writer->len = writer->pos < START_FIELDS ? START_FIELDS : writer->pos * 2;
        writer->terms = erealloc(writer->terms, writer->len * sizeof(writer->terms[0]));
        unless (writer->terms) {
            enif_free(old_ptr);
            writer->len = 0;
            return 0;
        }
    }
    writer->terms[writer->pos++] = term;
    return 1;
}

static int finish_current_buffer(writer_st_t *writer) {
    if (writer->current.pos) {
        unless (writer->current.pos == writer->current.bin.size ||
                (writer->current.pos < writer->current.bin.size &&
                 enif_realloc_binary(&writer->current.bin, writer->current.pos))) {
            enif_release_binary(&writer->current.bin);
            writer->current = (bin_buff_t){0};
            return 0;
        };
        ERL_NIF_TERM term = enif_make_binary(writer->env, &writer->current.bin);
        int success = add_term(writer, term);
        writer->current = (bin_buff_t){0};
        return success;
    }
    return 1;
}

static ERL_NIF_TERM return_result(writer_st_t *writer) {
    unless (finish_current_buffer(writer)) {
        release_writer_st(writer);
        return enif_raise_exception(writer->env, atoms.insufficient_memory);
    }
    ERL_NIF_TERM result = writer->pos == 1
        ? writer->terms[0]
        : enif_make_list_from_array(writer->env, writer->terms, writer->pos);
    release_writer_st(writer);
    return result;
}

static int resize_buffer(bin_buff_t *buf, size_t inc) {
    size_t size = buf->pos + inc;
    size = size < START_BUFFER_SIZE ? START_BUFFER_SIZE : clp2l(size);
    int success = buf->bin.size
        ? enif_realloc_binary(&buf->bin, size)
        : enif_alloc_binary(size, &buf->bin);
    if (success) {
        return 1;
    } else {
        if (buf->bin.size) enif_release_binary(&buf->bin);
        *buf = (bin_buff_t){0};
        return 0;
    }
}

static inline int grow_current_buffer(writer_st_t *writer, size_t inc) {
    return writer->current.pos + inc < writer->current.bin.size ||
        resize_buffer(&writer->current, inc);
}

static inline char *current_buffer_position(writer_st_t *writer) {
    return (char *)writer->current.bin.data + writer->current.pos;
}

static inline size_t free_space(writer_st_t *writer) {
    return writer->current.bin.size - writer->current.pos;
}

static inline void seek_current_buffer(writer_st_t *writer, size_t inc) {
    if (grow_current_buffer(writer, inc))
        writer->current.pos += inc;
}

static inline void putc_current_buffer_pos(writer_st_t *writer, char ch) {
    if (grow_current_buffer(writer, 1))
        writer->current.bin.data[writer->current.pos++] = ch;
}

#define grow_buffer(writer, size) \
    unless (grow_current_buffer((writer), (size))) { \
        enif_raise_exception(env, atoms.insufficient_memory); \
        return 0; \
    }

static int try_write_int(writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
    ErlNifSInt64 i;
    if (enif_get_int64(env, term, &i)) {
        grow_buffer(writer, chars4i(i)+1);
        seek_current_buffer(writer,
                snprintf(current_buffer_position(writer),
                    free_space(writer), "%li", i));
        return 1;
    } else {
        return 0;
    }
}

static int try_write_double(writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
    double d;
    if (enif_get_double(env, term, &d)) {
        grow_buffer(writer, FLOAT_SPACE);
        seek_current_buffer(writer,
                snprintf(current_buffer_position(writer),
                    free_space(writer), "%.16lg", d));
        return 1;
    } else {
        return 0;
    }
}

static int must_quote(const unsigned char *data, const size_t len)
{
    if (data[0] == CSV_SPACE || data[0] == CSV_TAB || data[len-1] == CSV_SPACE || data[len-1] == CSV_TAB)
        return 1;
    for (size_t i = 0; i < len; i++) {
        switch (data[i]) {
            case CSV_QUOTE :
            case CSV_COMMA :
            case CSV_CR :
            case CSV_LF :
                return 1;
        }
    }
    return 0;
}

static int try_write_atom(writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
#define ATOM_BUFF_LENGTH 256
    char atom_buff[ATOM_BUFF_LENGTH];
    unsigned atom_len;
    if ((atom_len = enif_get_atom(env, term, atom_buff,
                    ATOM_BUFF_LENGTH, ERL_NIF_LATIN1))) {
        if (must_quote((unsigned char *)atom_buff, atom_len-1)) {
            grow_buffer(writer, atom_len * 2 + 2);
            seek_current_buffer(writer,
                    csv_write(current_buffer_position(writer),
                        free_space(writer), atom_buff, atom_len-1));
        } else {
            grow_buffer(writer, atom_len);
            enif_get_atom(env, term, current_buffer_position(writer),
                    free_space(writer), ERL_NIF_LATIN1);
            seek_current_buffer(writer, atom_len-1);
        }
        return 1;
    } else {
        return 0;
    }
}

static int try_write_binary_or_iolist (writer_st_t *writer, ERL_NIF_TERM term) {
    ErlNifEnv *env = writer->env;
    ErlNifBinary bin;
    if (enif_inspect_binary(env, term, &bin) ||
            enif_inspect_iolist_as_binary(env, term, &bin)) {
        if (must_quote(bin.data, bin.size)) {
            grow_buffer(writer, bin.size * 2 + 3);
            seek_current_buffer(writer,
                    csv_write(current_buffer_position(writer), free_space(writer),
                        bin.data, bin.size));
        } else {
            unless (finish_current_buffer(writer) && add_term(writer, term)) {
                enif_raise_exception(env, atoms.insufficient_memory);
                return 0;
            }
        }
        return 1;
    } else {
        return 0;
    }
}

static int write_line(writer_st_t *writer, ERL_NIF_TERM list) {
    ErlNifEnv *env = writer->env;
    ERL_NIF_TERM head;
    const ERL_NIF_TERM comma = enif_make_uint(env, CSV_COMMA);
    const ERL_NIF_TERM lf    = enif_make_uint(env, CSV_LF);

    while (enif_get_list_cell(env, list, &head, &list)) {
        unless (try_write_int(writer, head) ||
                try_write_double(writer, head) ||
                try_write_atom(writer, head) ||
                try_write_binary_or_iolist(writer, head)) {
            unless (enif_has_pending_exception(env, NULL)) {
                enif_raise_exception(env, enif_make_tuple2(env, atoms.badarg, head));
            }
            return 0;
        }
        if (writer->current.bin.size) {
            putc_current_buffer_pos(writer, CSV_COMMA);
        } else unless (add_term(writer, comma)) {
            enif_raise_exception(env, atoms.insufficient_memory);
            return 0;
        }
    }
    if (writer->current.pos) {
        seek_current_buffer(writer, -1);
        putc_current_buffer_pos(writer, CSV_LF);
    } else if (writer->pos) {
        writer->terms[writer->pos-1] = lf;
    }

    unless (enif_is_empty_list(env, list)) {
        enif_raise_exception(env, enif_make_tuple2(env, atoms.badarg, list));
        return 0;
    }
    return 1;
}

NIF(write)
{
    writer_st_t writer;
    init_writer(env, &writer);
    ERL_NIF_TERM result;

    T_START;

    if (write_line(&writer, argv[0])) {
        result = return_result(&writer);
    } else {
        release_writer_st(&writer);
        enif_has_pending_exception(env, &result);
    }

    T_STOP;

    return result;
}

NIF(write_lines)
{
    writer_st_t writer;
    init_writer(env, &writer);
    ERL_NIF_TERM result, head, list = argv[0];

    T_START;

    while (enif_get_list_cell(env, list, &head, &list) && write_line(&writer, head)) {
    };

    if (enif_is_empty_list(env, list)) {
        result = return_result(&writer);
    } else {
        unless (enif_has_pending_exception(env, &result))
            result = enif_raise_exception(env, enif_make_tuple2(env, atoms.badarg, list));
        release_writer_st(&writer);
    }

    T_STOP;
    return result;
}

