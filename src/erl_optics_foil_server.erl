-module(erl_optics_foil_server).
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
    add_lens/2,
    stop/0
]).

-define(SERVER, ?MODULE).

-record(state, {
    new_lenses  :: list() | undefined,
    timer_ref :: timer:tref() | undefined | none
}).

%%%=========
%%% API
%%%=========

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%==========
%%% Callbacks
%%%==========

init(_) ->
    ets:new(erl_optics_ets, [named_table, public, {write_concurrency, false}, {read_concurrency, true}]),
    {ok, #state{new_lenses = [], timer_ref = none}, 0}.

handle_cast({add_lens, Lens}, State) ->
    case State#state.timer_ref of
        none ->
            Tref = timer:send_after(?ENV(?ENV_FOIL_RELOAD_INTERVAL,
                ?DEFAULT_FOIL_RELOAD_INTERVAL), reload_foil);
        _ ->
            Tref = State#state.timer_ref
    end,
    {noreply, #state{new_lenses =  [Lens | State#state.new_lenses],
        timer_ref = Tref}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_call(reload_foil, _From, State) ->
    maybe_reload_foil(State),
    {reply, ok, #state{new_lenses = []}};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(reload_foil, State) ->
    maybe_reload_foil(State),
    timer:cancel(State#state.timer_ref),
    {noreply, #state{new_lenses = [], timer_ref = none}};
handle_info(_, State) ->
    {noreply, State}.


%%% API

add_lens(Key, Ptr) ->
    gen_server:cast(?SERVER, {add_lens, {Key, Ptr}}).

terminate(_Reason, State) ->
    timer:cancel(State#state.timer_ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% private

insert_lens({Key, Ptr}) ->
    foil:insert(?NS, Key, Ptr).

maybe_reload_foil(State) ->
    Lenses = State#state.new_lenses,
    case Lenses of
        [] ->
            ok;
        _ ->
            lists:map(fun insert_lens/1, Lenses),
            foil:load(?NS)
    end.
