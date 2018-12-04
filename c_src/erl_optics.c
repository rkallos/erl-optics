#include "common.h"

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_n;
static ERL_NIF_TERM atom_p50;
static ERL_NIF_TERM atom_p90;
static ERL_NIF_TERM atom_p99;
static ERL_NIF_TERM atom_max;
static ERL_NIF_TERM atom_below;
static ERL_NIF_TERM atom_above;
static ERL_NIF_TERM atom_quantile;
static ERL_NIF_TERM atom_sample;
static ERL_NIF_TERM atom_sample_count;
static ERL_NIF_TERM atom_count;

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    atom_ok = enif_make_atom(env, "ok");
    atom_n = enif_make_atom(env, "n");
    atom_p50 = enif_make_atom(env, "p50");
    atom_p90 = enif_make_atom(env, "p90");
    atom_p99 = enif_make_atom(env, "p99");
    atom_max = enif_make_atom(env, "max");
    atom_below = enif_make_atom(env, "below");
    atom_above = enif_make_atom(env, "above");
    atom_quantile = enif_make_atom(env, "quantile");
    atom_sample = enif_make_atom(env, "sample");
    atom_sample_count = enif_make_atom(env, "sample_count");
    atom_count = enif_make_atom(env, "count");
    return 0;
}

static ERL_NIF_TERM eo_counter_alloc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    struct optics_lens *lens = optics_counter_alloc(optics, key);
    enif_free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_counter_alloc_get(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    struct optics_lens *lens = optics_counter_alloc_get(optics, key);
    enif_free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);  

  
} 

static ERL_NIF_TERM eo_counter_inc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    int64_t amt;
    if (!enif_get_int64(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_counter_inc(lens, amt)) return make_optics_error(env);

    return atom_ok;
}

static ERL_NIF_TERM eo_dist_alloc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    struct optics_lens *lens = optics_dist_alloc(optics, key);
    enif_free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_dist_alloc_get(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    struct optics_lens *lens = optics_dist_alloc_get(optics, key);
    enif_free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_dist_record(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    double amt;
    if (!enif_get_double(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_dist_record(lens, amt)) return make_optics_error(env);

    return atom_ok;
}

static ERL_NIF_TERM eo_gauge_alloc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    struct optics_lens *lens = optics_gauge_alloc(optics, key);
    enif_free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_gauge_alloc_get(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    struct optics_lens *lens = optics_gauge_alloc_get(optics, key);
    enif_free(key);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_gauge_set(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    double amt;
    if (!enif_get_double(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_gauge_set(lens, amt)) return make_optics_error(env);

    return atom_ok;
}

static ERL_NIF_TERM eo_histo_alloc(
    ErlNifEnv *env, int c, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    unsigned buckets_len;
    if (!enif_get_list_length(env, argv[2], &buckets_len))
        return ERROR("get_list_length");

    double *buckets = enif_alloc(buckets_len * sizeof(double));
    if (!buckets) return ERROR("enif_alloc");

    ERL_NIF_TERM head, tail;
    assert(enif_get_list_cell(env, argv[2], &head, &tail));

    size_t i = 0;
    do {
        assert(enif_get_double(env, head, &buckets[i]));
        ++i;
    } while(enif_get_list_cell(env, tail, &head, &tail));

    struct optics_lens *lens =
        optics_histo_alloc(optics, key, buckets, buckets_len);

    enif_free(buckets);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_histo_inc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    double amt;
    if (!enif_get_double(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_histo_inc(lens, amt)) return make_optics_error(env);

    return atom_ok;
}

static ERL_NIF_TERM eo_quantile_alloc(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    char *key = alloc_key(bin);
    if (!key) return ERROR("alloc_key");

    double target_quantile, estimate, adjustment_value;
    if (!enif_get_double(env, argv[2], &target_quantile) ||
        !enif_get_double(env, argv[3], &estimate) ||
        !enif_get_double(env, argv[4], &adjustment_value))
        return enif_make_badarg(env);

    struct optics_lens *lens =
        optics_quantile_alloc(optics, key, target_quantile,
                              estimate, adjustment_value);

    if (!lens) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)lens);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_quantile_update(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    double amt;
    if (!enif_get_double(env, argv[1], &amt)) {
        return enif_make_badarg(env);
    }

    if (!optics_quantile_update(lens, amt)) return make_optics_error(env);

    return atom_ok;
}

static ERL_NIF_TERM eo_lens_free(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    if(!optics_lens_free(lens)) return make_optics_error(env);

    return atom_ok;
}

static ERL_NIF_TERM eo_lens_close(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if(!lens) return ERROR("get_lens");

    optics_lens_close(lens);

    return atom_ok;
}

static ERL_NIF_TERM eo_optics_create(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    char* name = alloc_key(bin);
    struct optics *optics = optics_create(name);
    if (!optics) return make_optics_error(env);

    ERL_NIF_TERM ptr = enif_make_int64(env, (int64_t)optics);

    return enif_make_tuple2(env, atom_ok, ptr);
}

static ERL_NIF_TERM eo_optics_free(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics *optics = get_optics(env, argv[0]);
    if (!optics) return ERROR("get_optics");
    optics_close(optics);

    return atom_ok;
}


                           

//static ERL_NIF_TERM eo_set_prefix(
//    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
//{
//    char *prefix;
//    
//    enif_get_string(env, argv[0], prefix, strlen(argv[0]), ERL_NIF_LATIN1); 
//    optics_set_prefix(prefix);
//    return atom_ok;
//}

//------------------------------------------------------------------------------
// For testing: DO NOT USE IN PRODUCTION CODE
//------------------------------------------------------------------------------


/*
static ERL_NIF_TERM eo_counter_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    int64_t val = 0;

    switch(optics_counter_read(lens, optics_epoch(lens->optics), &val)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return ERROR("optics_busy");
    case optics_break:
        return ERROR("optics_break");
    case optics_ok:
        break;
    }
    return enif_make_int64(env, val);
}

static ERL_NIF_TERM eo_dist_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    struct optics_dist dist = {0};

    switch(optics_dist_read(lens, optics_epoch(lens->optics), &dist)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return ERROR("optics_busy");
    case optics_break:
        return ERROR("optics_break");
    case optics_ok:
        break;
    }

    ERL_NIF_TERM v_n = enif_make_uint64(env, dist.n);
    ERL_NIF_TERM v_p50 = enif_make_double(env, dist.p50);
    ERL_NIF_TERM v_p90 = enif_make_double(env, dist.p90);
    ERL_NIF_TERM v_p99 = enif_make_double(env, dist.p99);
    ERL_NIF_TERM v_max = enif_make_double(env, dist.max);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, atom_n, v_n, &map);
    enif_make_map_put(env, map, atom_p50, v_p50, &map);
    enif_make_map_put(env, map, atom_p90, v_p90, &map);
    enif_make_map_put(env, map, atom_p99, v_p99, &map);
    enif_make_map_put(env, map, atom_max, v_max, &map);
    return map;
}

static ERL_NIF_TERM eo_gauge_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    double val = 0.0;

    switch(optics_gauge_read(lens, optics_epoch(lens->optics), &val)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return ERROR("optics_busy");
    case optics_break:
        return ERROR("optics_break");
    case optics_ok:
        break;
    }
    return enif_make_double(env, val);
}

static ERL_NIF_TERM eo_histo_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    struct optics_histo histo = {0};

    switch(optics_histo_read(lens, optics_epoch(lens->optics), &histo)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return ERROR("optics_busy");
    case optics_break:
        return ERROR("optics_break");
    case optics_ok:
        break;
    }

    ERL_NIF_TERM below = enif_make_uint64(env, histo.below);
    ERL_NIF_TERM above = enif_make_uint64(env, histo.above);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, atom_below, below, &map);
    enif_make_map_put(env, map, atom_above, above, &map);

    for(size_t i = 0; i < histo.buckets_len - 1; ++i) {
        ERL_NIF_TERM k = enif_make_double(env, histo.buckets[i]);
        ERL_NIF_TERM v = enif_make_uint64(env, histo.counts[i]);
        enif_make_map_put(env, map, k, v, &map);
    }

    return map;
}

static ERL_NIF_TERM eo_quantile_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct optics_lens *lens = get_lens(env, argv[0]);
    if (!lens) return ERROR("get_lens");

    struct optics_quantile val = {0};

    switch(optics_quantile_read(lens, optics_epoch(lens->optics), &val)) {
    case optics_err:
        return make_optics_error(env);
    case optics_busy:
        return ERROR("optics_busy");
    case optics_break:
        return ERROR("optics_break");
    case optics_ok:
        break;
    }

    ERL_NIF_TERM quantile = enif_make_double(env, val.quantile);
    ERL_NIF_TERM sample = enif_make_double(env, val.sample);
    ERL_NIF_TERM sample_count = enif_make_uint64(env, val.sample_count);
    ERL_NIF_TERM count = enif_make_uint64(env, val.count);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, atom_quantile, quantile, &map);
    enif_make_map_put(env, map, atom_sample, sample, &map);
    enif_make_map_put(env, map, atom_sample_count, sample_count, &map);
    enif_make_map_put(env, map, atom_count, count, &map);
    return map;
}
*/


static ErlNifFunc nif_funcs[] =
{
    {"counter_alloc", 2, eo_counter_alloc},
    {"counter_alloc_get", 2, eo_counter_alloc_get},
    {"counter_inc", 2, eo_counter_inc},

    {"dist_alloc", 2, eo_dist_alloc},
    {"dist_alloc_get", 2, eo_dist_alloc_get},
    {"dist_record", 2, eo_dist_record},

    {"gauge_alloc", 2, eo_gauge_alloc},
    {"gauge_alloc_get", 2, eo_gauge_alloc_get},    
    {"gauge_set", 2, eo_gauge_set},

    {"histo_alloc", 3, eo_histo_alloc},
    {"histo_inc", 2, eo_histo_inc},

    {"lens_free", 1, eo_lens_free},
    {"lens_close", 1, eo_lens_close},
    {"optics_create", 1, eo_optics_create},
    {"optics_free", 1, eo_optics_free},

    {"quantile_alloc", 5, eo_quantile_alloc},
    {"quantile_update", 2, eo_quantile_update},
    //{"set_prefix", 1, eo_set_prefix},

    // For testing
    // TODO: Split into a separate NIF
    //{"counter_read", 1, eo_counter_read},
    //{"dist_read", 1, eo_dist_read},
    //{"gauge_read", 1, eo_gauge_read},
    //{"histo_read", 1, eo_histo_read},
    //{"quantile_read", 1, eo_quantile_read},
};

ERL_NIF_INIT(erl_optics_nif, nif_funcs, load, NULL, NULL, NULL)

// TODO: Pre-create atoms?
