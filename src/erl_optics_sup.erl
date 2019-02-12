-module(erl_optics_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Server = #{id => erl_optics_server,
               start => {erl_optics_server, start_link, [carbon]},
               shutdown => 2000,
               restart => permanent,
               type => worker,
               modules => [erl_optics_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 1, 1},
    {ok, {RestartStrategy, Children}}.
