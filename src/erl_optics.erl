-module(erl_optics).

-define(NS, ?MODULE).

-export([counter_inc/1,
         counter_inc/2,
         dist_record/2,
         gauge_set/2,
         lens_free/1,
         start/0,
         start/1,
         stop/0]).

-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key, Amt) ->
    {ok, Ptr} = get_lens(Key),
    erl_optics_nif:counter_inc(Ptr, Amt).


-spec dist_record(binary(), float()) -> ok | {error, term()}.

dist_record(Key, Val) ->
    {ok, Ptr} = get_lens(Key),
    erl_optics_nif:dist_record(Ptr, Val).


-spec gauge_set(binary(), number()) -> ok | {error, term()}.

gauge_set(Key, Val) when is_integer(Val) ->
    gauge_set(Key, float(Val));

gauge_set(Key, Val) ->
    {ok, Ptr} = get_lens(Key),
    erl_optics_nif:gauge_set(Ptr, Val).


-spec lens_free(binary()) -> ok | {error, term()}.

lens_free(Key) ->
    {ok, Lens} = get_lens(Key),
    ok = erl_optics_nif:lens_free(Lens),
    foil:delete(?NS, Key),
    foil:load(?NS).


-spec start() -> ok.

start() ->
    Lenses = [erl_optics_lens:counter(<<"bob_the_counter">>),
              erl_optics_lens:gauge(<<"bob_the_gauge">>),
              erl_optics_lens:dist(<<"bob_the_dist">>)],
    start(Lenses).

-spec start([erl_optics_lens:desc()]) -> ok.

start(Lenses) ->
    case create_foil() of
        ok ->
            ok = create_optics(),
            ok = alloc_lenses(Lenses);
        Err -> Err
    end.


-spec stop() -> ok.

stop() ->
    {ok, Ptr} = get_optics(),
    ok = erl_optics_nif:optics_free(Ptr),
    ok = foil:delete(?NS).

%% private

alloc_lenses([]) ->
    foil:load(?NS);

alloc_lenses([{Name, Type} | Rest]) ->
    {ok, Ptr} = case Type of
        counter -> alloc_counter(Name);
           dist -> alloc_dist(Name);
          gauge -> alloc_gauge(Name)
    end,
    foil:insert(?NS, Name, Ptr),
    alloc_lenses(Rest).


alloc_counter(Name) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:alloc_counter(Optics, Name).


alloc_dist(Name) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:alloc_dist(Optics, Name).


alloc_gauge(Name) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:alloc_gauge(Optics, Name).


create_foil() ->
    application:ensure_all_started(foil),
    case foil:new(?MODULE) of
        {error, module_exists} ->
            {error, already_started};
        ok -> ok
    end.

create_optics() ->
    OpticsStatus = erl_optics_nif:optics_alloc(),
    case OpticsStatus of
        {ok, Ptr} ->
            ok = foil:insert(?MODULE, optics, Ptr),
            ok = foil:load(?MODULE);
        _ ->
            {error, cannot_alloc_optics}
    end.


get_lens(Key) ->
    foil:lookup(?NS, Key).


get_optics() ->
    foil:lookup(?NS, optics).
