-module(erl_optics_server).
-include("erl_optics.hrl").

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/1,
         stop/0]).

%for prototyping only
-export([start/0,
         test_update/0,
         start_test/1]).

-define(SERVER, ?MODULE).

-record(state, {port = ?ENV_PORT     :: inet:port_number(),
                addr = ?ENV_HOSTNAME :: inet:socket_address() | inet:hostname(),
                mode                 :: carbon | prometheus | erlang}).


%%%=========
%%% API
%%%=========

%Modes: prometheus | {carbon, interval=integer()}

start_link(Mode) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mode], []).

stop() ->
    gen_server:cast(?SERVER, stop).

start_test(Interval) ->
    gen_server:cast(?SERVER, {test_update, Interval}).

%%%==========
%%% Callbacks
%%%==========

init([Mode]) ->
    case Mode of
        carbon ->
            erl_optics:allocate_carbon_poller("localhost", "1055"),
            timer:send_interval(?DEFAULT_INTERVAL, carbon_poll),
            gen_server:cast(?SERVER, {test_update, 10}), %for testing
            {ok, #state{mode = carbon}, 0};
        prometheus ->
            %to be implemented
            {ok, #state{mode = prometheus}, 0};
        erlang ->
            timer:send_interval(?DEFAULT_INTERVAL, erlang_poll),
            {ok, #state{mode = erlang}, 0}
    end.

handle_call(erlang_poll, _From, State) ->
    {reply, erl_optics:poll(), State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({test_update, Interval}, State) ->
    timer:send_interval(Interval, test_update),
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State};
handle_info(test_update, State) ->
    test_update(),
    {noreply, State};
handle_info(carbon_poll, State) ->
    erl_optics:poll(),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================
%%% Internal functions
%%%===================

poll() ->
    {ok, Map} = erl_optics:poll(),
    {ok, {Map, os:timestamp()}}.

start() ->
    Lenses = [
        erl_optics_lens:counter(<<"bob_the_counter">>),
        erl_optics_lens:dist(<<"bob_the_dist">>),
        erl_optics_lens:gauge(<<"bob_the_gauge">>),
        erl_optics_lens:histo(<<"bob_the_histo">>, [0, 10, 20, 30, 40]),
        erl_optics_lens:quantile(<<"bob_the_quantile">>, 0.5, 0.0, 0.01)
    ],
    erl_optics:start(<<"test">>, Lenses).

test_update() ->
    erl_optics:counter_inc(<<"bob_the_counter">>),
    %erl_optics:dist_record_timing_now_us(<<"bob_the_dist">>, os:timestamp()),
    %erl_optics:dist_record(<<"bob_the_dist">>, rand:normal()),
    erl_optics:dist_record(<<"bob_the_dist">>, 1.0),
    erl_optics:gauge_set(<<"bob_the_gauge">>, rand:uniform()),
    %erl_optics:quantile_update_timing_now_us(<<"bob_the_quantile">>, os:timestamp() ),
    erl_optics:quantile_update(<<"bob_the_quantile">>, rand:uniform()),
    erl_optics:histo_inc(<<"bob_the_histo">>, rand:uniform() * 40.0),
    ok.
