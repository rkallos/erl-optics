#include "common.h"

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

    double amt;
    if (!enif_get_double(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_gauge_set(lens, amt)) return make_optics_error(env);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM lens_free(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

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

static ERL_NIF_TERM epoch(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return make_error(env, "get_optics");

    size_t epoch = optics_epoch(optics);

    return enif_make_int64(env, epoch);
}

static ERL_NIF_TERM optics_free(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return make_error(env, "get_optics");
    optics_close(optics);

    return enif_make_atom(env, "ok");
}

//------------------------------------------------------------------------------
// For testing: DO NOT USE IN PRODUCTION CODE
//------------------------------------------------------------------------------

static ERL_NIF_TERM counter_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

    size_t epoch;
    if (!enif_get_uint64(env, argv[1], &epoch))
        return make_error(env, "enif_get_uint64");

    int64_t val;

    switch(optics_counter_read(lens, epoch, &val)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return make_error(env, "optics_busy");
    case optics_break:
        return make_error(env, "optics_break");
    }
    return enif_make_uint64(env, val);
}

static ERL_NIF_TERM dist_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

    size_t epoch;
    if (!enif_get_uint64(env, argv[1], &epoch))
        return make_error(env, "enif_get_uint64");

    struct optics_dist val;

    switch(optics_dist_read(lens, epoch, &val)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return make_error(env, "optics_busy");
    case optics_break:
        return make_error(env, "optics_break");
    }

    ERL_NIF_TERM k_n = enif_make_atom(env, "n");
    ERL_NIF_TERM k_p50 = enif_make_atom(env, "p50");
    ERL_NIF_TERM k_p90 = enif_make_atom(env, "p90");
    ERL_NIF_TERM k_p99 = enif_make_atom(env, "p99");
    ERL_NIF_TERM k_max = enif_make_atom(env, "max");

    ERL_NIF_TERM v_n = enif_make_uint64(env, val.n);
    ERL_NIF_TERM v_p50 = enif_make_double(env, val.p50);
    ERL_NIF_TERM v_p90 = enif_make_double(env, val.p90);
    ERL_NIF_TERM v_p99 = enif_make_double(env, val.p99);
    ERL_NIF_TERM v_max = enif_make_double(env, val.max);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, k_n, v_n, &map);
    enif_make_map_put(env, map, k_p50, v_p50, &map);
    enif_make_map_put(env, map, k_p90, v_p90, &map);
    enif_make_map_put(env, map, k_p99, v_p99, &map);
    enif_make_map_put(env, map, k_max, v_max, &map);
    return map;
}

static ERL_NIF_TERM gauge_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return make_error(env, "get_lens");

    size_t epoch;
    if (!enif_get_uint64(env, argv[1], &epoch))
        return make_error(env, "enif_get_uint64");

    double val;

    switch(optics_gauge_read(lens, epoch, &val)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return make_error(env, "optics_busy");
    case optics_break:
        return make_error(env, "optics_break");
    }
    return enif_make_double(env, val);
}

static ErlNifFunc nif_funcs[] =
{
    {"alloc_counter", 2, alloc_counter},
    {"alloc_dist", 2, alloc_dist},
    {"alloc_gauge", 2, alloc_gauge},
    {"counter_inc", 2, counter_inc},
    {"dist_record", 2, dist_record},
    {"gauge_set", 2, gauge_set},
    {"lens_free", 1, lens_free},
    {"optics_alloc", 0, optics_alloc},
    {"optics_epoch", 1, epoch},
    {"optics_free", 1, optics_free},

    // For testing
    // TODO: Split into a separate NIF
    {"counter_read", 2, counter_read},
    {"dist_read", 2, dist_read},
    {"gauge_read", 2, gauge_read}
};

ERL_NIF_INIT(erl_optics_nif, nif_funcs, NULL, NULL, NULL, NULL)

// TODO: Pre-create atoms?
