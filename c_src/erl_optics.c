#include "optics.h"
#include "erl_nif.h"

#include <stdint.h>
#include <string.h>

static ERL_NIF_TERM make_error(ErlNifEnv* env, const char *_msg)
{
    ERL_NIF_TERM err = enif_make_atom(env, "error");
    ERL_NIF_TERM msg = enif_make_atom(env, _msg);
    return enif_make_tuple2(env, err, msg);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    struct optics *bob = optics_create("bob");
    if (!bob) return make_error(env, "optics_create_failed");

    *priv_data = bob;
    return 0;
}

static void unload(ErlNifEnv* env, void *priv_data)
{
    struct optics *optics = (struct optics *) priv_data;
    optics_close(optics);
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

static ERL_NIF_TERM counter_inc(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    int64_t amt;
    if (!enif_get_int64(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return make_error(env, "alloc_key");

    struct optics *optics = (struct optics *) enif_priv_data(env);
    struct optics_lens *lens = optics_counter_alloc_get(optics, key);
    free(key);

    if (!lens) {
        return make_error(env, "optics_lens_alloc_get");
    }
    optics_counter_inc(lens, amt);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM lens_free(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return make_error(env, "alloc_key");

    struct optics *optics = (struct optics *) enif_priv_data(env);
    struct optics_lens *lens = optics_counter_alloc_get(optics, key);
    free(key);

    if (!lens) {
        return make_error(env, "optics_lens_alloc_get");
    }
    optics_lens_free(lens);

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"counter_inc", 2, counter_inc},
    {"lens_free", 1, lens_free}
};

ERL_NIF_INIT(erl_optics, nif_funcs, load, NULL, NULL, unload)
