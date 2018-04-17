-module(erl_optics).

-export([init/0]).

-export([counter_inc/1,
         lens_free/1]).

counter_inc(_Key) ->
    exit(nif_library_not_loaded).

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

lens_free(_Key) ->
    exit(nif_library_not_loaded).
