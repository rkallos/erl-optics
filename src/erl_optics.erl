-module(erl_optics).

-export([init/0]).

-export([counter_inc/1,
         counter_inc/2,
         dist_record/2,
         gauge_set/2,
         lens_free/1]).

-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(_Key, _Amt) ->
    exit(nif_library_not_loaded).


-spec dist_record(binary(), float()) -> ok | {error, term()}.

dist_record(_Key, _Val) ->
    exit(nif_library_not_loaded).


-spec gauge_set(binary(), number()) -> ok | {error, term()}.

gauge_set(Key, Val) when is_integer(Val) ->
    gauge_set2(Key, float(Val));

gauge_set(Key, Val) ->
    gauge_set2(Key, Val).


-spec gauge_set2(binary(), float()) -> ok | {error, term()}.

gauge_set2(_Key, _Val) ->
    exit(nif_library_not_loaded).


-spec init() -> ok.

init() ->
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


-spec lens_free(binary()) -> ok | {error, term()}.

lens_free(_Key) ->
    exit(nif_library_not_loaded).
