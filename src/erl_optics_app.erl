-module(erl_optics_app).

-behaviour(application).

-export([
    start/2,
    stop/1
    ]).

start(_Type, _StartArgs) ->
    case erl_optics_sup:start_link() of
        {ok, Pid} ->
            io:format("~p~n", [Pid]),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
