-module(erl_optics).

-export([init/0]).

-export([counter_inc/1,
         counter_inc/2,
         gauge_set/2,
         lens_free/1]).

-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).


-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(_Key, _Amt) ->
    exit(nif_library_not_loaded).


-spec gauge_set(binary(), float()) -> ok | {error, term()}.

gauge_set(_Key, _Val) ->
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
