#include "optics.h"
#include "erl_nif.h"

#include <assert.h>
#include <stdint.h>
#include <string.h>

#define ERROR(msg) make_error(env, msg, __FILE__, __LINE__);

static ERL_NIF_TERM make_error(
    ErlNifEnv *env, const char *_msg, const char *f, int l)
{
    ERL_NIF_TERM err = enif_make_atom(env, "error");
    ERL_NIF_TERM file = enif_make_atom(env, f);
    ERL_NIF_TERM line = enif_make_uint(env, l);
    ERL_NIF_TERM msg = enif_make_atom(env, _msg);
    return enif_make_tuple4(env, err, msg, file, line);
}

static ERL_NIF_TERM make_optics_error(ErlNifEnv *env)
{
    char buf[4096];
    optics_strerror(&optics_errno, buf, 4096);
    ERL_NIF_TERM err = enif_make_atom(env, "error");
    ERL_NIF_TERM msg = enif_make_string(env, buf, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, err, msg);
}

static char *alloc_key(ErlNifBinary bin)
{
    size_t key_len;
    if (bin.size > optics_name_max_len) key_len = optics_name_max_len;
    else key_len = bin.size;

    char *key = enif_alloc(key_len);
    if (!key) return NULL;

    memcpy(key, bin.data, key_len);
    key[key_len] = 0;

    return key;
}

static void *get_ptr(ErlNifEnv *env, ERL_NIF_TERM arg)
{
    int64_t dst;
    if (!enif_get_int64(env, arg, &dst)) return NULL;
    return (void *)dst; // Nothing to see here. Move along.
}

static struct optics *get_optics(ErlNifEnv *env, ERL_NIF_TERM arg)
{
    return (struct optics *)get_ptr(env, arg);
}

static struct optics_lens *get_lens(ErlNifEnv *env, ERL_NIF_TERM arg)
{
    return (struct optics_lens *)get_ptr(env, arg);
}
