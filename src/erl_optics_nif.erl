-module(erl_optics_nif).

-compile(no_native).
-on_load(on_load/0).

-export([
    alloc_counter/2,
    alloc_dist/2,
    alloc_gauge/2,
    counter_inc/2,
    dist_record/2,
    gauge_set/2,
    lens_free/1,
    optics_alloc/0,
    optics_epoch/1,
    optics_free/1,
    counter_read/2,
    dist_read/2,
    gauge_read/2
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

alloc_counter(_Optics, _Name) ->
    ?nif_stub.
alloc_dist(_Optics, _Name) ->
    ?nif_stub.
alloc_gauge(_Optics, _Name) ->
    ?nif_stub.
alloc_histo(_Optics, _Name, _Buckets) ->
    ?nif_stub.
counter_inc(_K, _V) ->
    ?nif_stub.
dist_record(_K, _V) ->
    ?nif_stub.
gauge_set(_K, _V) ->
    ?nif_stub.
histo_inc(_K, _V) ->
    ?nif_stub.
lens_free(_K) ->
    ?nif_stub.
optics_alloc() ->
    ?nif_stub.
optics_epoch(_Ptr) ->
    ?nif_stub.
optics_free(_Ptr) ->
    ?nif_stub.
counter_read(_Lens, _Epoch) ->
    ?nif_stub.
dist_read(_Lens, _Epoch) ->
    ?nif_stub.
gauge_read(_Lens, _Epoch) ->
    ?nif_stub.
histo_read(_Lens, _Epoch) ->
    ?nif_stub.