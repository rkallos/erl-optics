#include "optics.h"
#include "erl_nif.h"

#include <stdint.h>
#include <string.h>

static ERL_NIF_TERM make_error(ErlNifEnv *env, const char *_msg)
{
    ERL_NIF_TERM err = enif_make_atom(env, "error");
    ERL_NIF_TERM msg = enif_make_atom(env, _msg);
    return enif_make_tuple2(env, err, msg);
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

    char *key = malloc(key_len);
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

static ERL_NIF_TERM alloc_counter(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return make_error(env, "get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return make_error(env, "alloc_key");

    struct optics_lens *lens = optics_counter_alloc(optics, key);
    free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, ok, ptr);
}

static ERL_NIF_TERM alloc_dist(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return make_error(env, "get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return make_error(env, "alloc_key");

    struct optics_lens *lens = optics_dist_alloc(optics, key);
    free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, ok, ptr);
}

static ERL_NIF_TERM alloc_gauge(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return make_error(env, "get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return make_error(env, "alloc_key");

    struct optics_lens *lens = optics_gauge_alloc(optics, key);
    free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, ok, ptr);
}

static ERL_NIF_TERM counter_inc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

    int64_t amt;
    if (!enif_get_int64(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_counter_inc(lens, amt)) return make_optics_error(env);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM dist_record(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

    double amt;
    if (!enif_get_double(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_dist_record(lens, amt)) return make_optics_error(env);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM gauge_set(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

    int64_t amt;
    if (!enif_get_int64(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_gauge_set(lens, amt)) return make_optics_error(env);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM lens_free(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return make_error(env, "alloc_key");

    struct optics *optics = (struct optics *) enif_priv_data(env);
    struct optics_lens *lens = optics_lens_get(optics, key);
    free(key);

    if (!lens) return make_optics_error(env);

    if (!optics_lens_free(lens)) return make_optics_error(env);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM optics_alloc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = optics_create("erl_optics");
    if (!optics) return make_optics_error(env);

    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)optics);

    return enif_make_tuple2(env, ok, ptr);
}

static ERL_NIF_TERM optics_free(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return make_error(env, "get_optics_ptr");
    optics_close(optics);

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"alloc_counter_nif", 2, alloc_counter},
    {"alloc_dist_nif", 2, alloc_dist},
    {"alloc_gauge_nif", 2, alloc_gauge},
    {"counter_inc_nif", 2, counter_inc},
    {"dist_record_nif", 2, dist_record},
    {"gauge_set_nif", 2, gauge_set},
    {"lens_free_nif", 1, lens_free},
    {"optics_alloc_nif", 0, optics_alloc},
    {"optics_free_nif", 1, optics_free}
};

ERL_NIF_INIT(erl_optics, nif_funcs, NULL, NULL, NULL, NULL)

// TODO: Pre-create atoms?
