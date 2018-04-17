#include "optics.h"
#include "erl_nif.h"

#include <stdint.h>

ERL_NIF_TERM make_error(ErlNifEnv* env, const char *_msg)
{
    ERL_NIF_TERM err = enif_make_atom(env, "error");
    ERL_NIF_TERM msg = enif_make_atom(env, _msg);
    return enif_make_tuple2(env, err, msg);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    struct optics *bob = optics_create("bob");
    if (!bob) return make_error(env, "optics_create_failed");

    struct optics_lens *lens = optics_counter_alloc(bob, "bob_the_counter");
    if (!lens) return make_error(env, "optics_counter_alloc_failed");

    *priv_data = bob;
    return 0;
}

static void unload(ErlNifEnv* env, void *priv_data)
{
    struct optics *optics = (struct optics *) priv_data;
    struct optics_lens *lens = optics_lens_get(optics, "bob_the_counter");
    optics_lens_close(lens);
    optics_close(optics);
}

static ERL_NIF_TERM increment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void *data = enif_priv_data(env);
    struct optics *optics = (struct optics *) data;
    struct optics_lens *lens = optics_lens_get(optics, "bob_the_counter");
    if (!lens) {
        return make_error(env, "optics_lens_get");
    }
    optics_counter_inc(lens, 1);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM read_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void *data = enif_priv_data(env);
    struct optics *optics = (struct optics *) data;
    struct optics_lens *lens = optics_lens_get(optics, "bob_the_counter");
    int64_t val = 0;
    enum optics_ret r = optics_counter_read(lens, optics_epoch(optics), &val);

    if (r == optics_ok) {
        ERL_NIF_TERM ok = enif_make_atom(env, "ok");
        return enif_make_tuple2(env, ok, enif_make_int64(env, val));
    } else {
        return make_error(env, "cannot_read_counter");
    }
}

static ErlNifFunc nif_funcs[] =
{
    {"increment", 0, increment},
    {"read_counter", 0, read_counter}
};

ERL_NIF_INIT(erl_optics, nif_funcs, load, NULL, NULL, unload)
