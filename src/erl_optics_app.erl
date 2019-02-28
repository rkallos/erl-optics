-module(erl_optics_app).
-include("erl_optics.hrl").

-behaviour(application).

-export([
    start/2,
    start/0,
    stop/1
    ]).

start(_Type, _StartArgs) ->
    case erl_optics_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    supervisor:terminate_child(erl_optics_sup, erl_optics_server),
    exit(whereis(erl_optics_sup), kill),
    ok.

start() ->
    application:ensure_all_started(?APP).
