-module(erl_optics_server).
-include("erl_optics.hrl").

-behaviour(gen_server).

-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0,
    stop/0
]).

-define(SERVER, ?MODULE).

-record(state, {
    port  :: undefined | non_neg_integer(),
    addr  :: undefined | list(),
    mode  :: undefined | carbon | blank
}).


%%%=========
%%% API
%%%=========

%Modes: carbon | blank

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%==========
%%% Callbacks
%%%==========

init(_) ->
    Mode = ?ENV(?ENV_MODE, ?DEFAULT_MODE),
    case Mode of
        carbon ->
            Hostname = ?ENV(?ENV_HOSTNAME, ?DEFAULT_HOSTNAME),
            Port = ?ENV(?ENV_PORT, ?DEFAULT_PORT),
            Interval = ?ENV(?ENV_INTERVAL, ?DEFAULT_INTERVAL),
            erl_optics:register_carbon_poller(Hostname, Port),
            timer:send_interval(Interval, carbon_poll),
            {ok, #state{mode = Mode, port = Port, addr = Hostname}, 0};
        blank ->
            {ok, #state{mode = Mode}, 0}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(carbon_poll, State) ->
    Mode = State#state.mode,
    case Mode of
        carbon ->
            erl_optics:poll(),
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
