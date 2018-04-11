#include "optics.h"
#include "erl_nif.h"

#include <stdint.h>

static ERL_NIF_TERM increment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int64_t i = 1;
    return enif_make_int64(env, i);
}

static ErlNifFunc nif_funcs[] =
{
    {"increment", 0, increment}
};

ERL_NIF_INIT(erl_optics, nif_funcs, NULL, NULL, NULL, NULL)
