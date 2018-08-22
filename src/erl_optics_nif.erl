-module(erl_optics_nif).

-compile(no_native).
-on_load(on_load/0).

-export([
    counter_alloc/2,
    counter_inc/2,
    dist_alloc/2,
    dist_record/2,
    gauge_alloc/2,
    gauge_set/2,
    histo_alloc/3,
    histo_inc/2,
    lens_free/1,
    optics_create/0,
    optics_free/1,
    quantile_alloc/5,
    quantile_update/2
]).

%% for testing purposes only
-export([
    counter_read/1,
    dist_read/1,
    gauge_read/1,
    histo_read/1,
    quantile_read/1
]).

%% THIS MODULE SHOULD ONLY BE CALLED FROM erl_optics.erl
%% OTHERWISE, A CYBER-SHARK MIGHT EAT YOUR ERLANG NODE.

-spec on_load() -> ok.

on_load() ->
    SoName = case code:priv_dir(erl_optics) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, erl_optics]);
                _ ->
                    filename:join([priv, erl_optics])
            end;
        Dir ->
            filename:join(Dir, erl_optics)
    end,
    ok = erlang:load_nif(SoName, 0).

%% shamelessly stolen from crypto.erl
-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

counter_alloc(_Optics, _Name) ->
    ?nif_stub.
counter_inc(_K, _V) ->
    ?nif_stub.
counter_read(_Lens) ->
    ?nif_stub.
dist_alloc(_Optics, _Name) ->
    ?nif_stub.
dist_read(_Lens) ->
    ?nif_stub.
dist_record(_K, _V) ->
    ?nif_stub.
gauge_alloc(_Optics, _Name) ->
    ?nif_stub.
gauge_read(_Lens) ->
    ?nif_stub.
gauge_set(_K, _V) ->
    ?nif_stub.
histo_alloc(_Optics, _Name, _Buckets) ->
    ?nif_stub.
histo_inc(_K, _V) ->
    ?nif_stub.
histo_read(_Lens) ->
    ?nif_stub.
lens_free(_K) ->
    ?nif_stub.
optics_create() ->
    ?nif_stub.
optics_epoch(_Optics) ->
    ?nif_stub.
optics_free(_Ptr) ->
    ?nif_stub.
quantile_alloc(_Optics, _Name, _Target, _Estimate, _Adjustment) ->
    ?nif_stub.
quantile_read(_Lens) ->
    ?nif_stub.
quantile_update(_K, _V) ->
    ?nif_stub.
