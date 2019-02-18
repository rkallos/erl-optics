-module(erl_optics).

-define(NS, ?MODULE).

-export([
    counter_inc/1,
    counter_inc/2,
    counter_inc_alloc/1,
    counter_inc_alloc/2,
    dist_record/2,
    dist_record_alloc/2,
    dist_record_timing_now_us/2,
    dist_record_timing_now/2,
    gauge_set/2,
    gauge_set_alloc/2,
    histo_inc/2,
    lens_update/2,
    lens_free/1,
    quantile_update/2,
    quantile_update_timing_now/2,
    quantile_update_timing_now_us/2,
    start/2,
    start_optics/2,
    stop/0,
    poll/0,
    register_carbon_poller/2,
    register_erlang_poller/0
]).


-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key, Amt) ->
    case get_lens(Key) of
        {ok, Ptr} ->
            erl_optics_nif:counter_inc(Ptr, Amt);
        {error, Msg} ->
            {error, Msg}
    end.



-spec counter_inc_alloc(binary()) -> ok | {error, term()}.

counter_inc_alloc(Key) ->
    counter_inc_alloc(Key, 1).

-spec counter_inc_alloc(binary(), integer()) -> ok | {error, term()}.

counter_inc_alloc(Key, Amt)->
    {ok, OpticsPtr} = get_optics(),
    {ok, Ptr} = erl_optics_nif:counter_alloc_get(OpticsPtr, Key),
    erl_optics_nif:counter_inc(Ptr, Amt),
    erl_optics_nif:lens_close(Ptr).


-spec dist_record(binary(), number()) -> ok | {error, term()}.
dist_record(Key, Val) when is_integer(Val) ->
    dist_record(Key, float(Val));

dist_record(Key, Val) ->
    case get_lens(Key) of
        {ok, Ptr} ->
            erl_optics_nif:dist_record(Ptr, Val);
        {error, Msg} ->
            {error, Msg}
    end.

-spec dist_record_alloc(binary(), float()) -> ok | {error, term()}.

dist_record_alloc(Key, Val) ->
    {ok, OpticsPtr} = get_optics(),
    {ok, Ptr} = erl_optics_nif:dist_alloc_get(OpticsPtr, Key),
    erl_optics_nif:dist_record(Ptr, Val),
    erl_optics_nif:lens_close(Ptr).

-spec dist_record_timing_now_us(binary(), erlang:timestamp()) -> ok | {error, term()}.

dist_record_timing_now_us(Key, Stamp) ->
    Delta = float(timer:now_diff(os:timestamp(), Stamp)),
    dist_record(Key, Delta).

-spec dist_record_timing_now(binary(), erlang:timestamp()) -> ok | {error, term()}.

dist_record_timing_now(Key, Stamp) ->
    Delta = float(timer:now_diff(os:timestamp(), Stamp)) / 1000.0,
    dist_record(Key, Delta).

-spec gauge_set(binary(), number()) -> ok | {error, term()}.

gauge_set(Key, Val) when is_integer(Val) ->
    gauge_set(Key, float(Val));

gauge_set(Key, Val) ->
    case get_lens(Key) of
        {ok, Ptr} ->
            erl_optics_nif:gauge_set(Ptr, Val);
        {error, Msg} ->
            {error, Msg}
    end.

-spec gauge_set_alloc(binary(), number()) -> ok | {error, term()}.

gauge_set_alloc(Key, Val) when is_integer(Val) ->
    gauge_set_alloc(Key, float(Val));

gauge_set_alloc(Key, Val) ->
    {ok, OpticsPtr} = get_optics(),
    {ok, Ptr} = erl_optics_nif:gauge_alloc_get(OpticsPtr, Key),
    erl_optics_nif:gauge_set(Ptr, Val),
    erl_optics_nif:lens_close(Ptr).


-spec histo_inc(binary(), number()) -> ok | {error, term()}.

histo_inc(Key, Val) when is_integer(Val) ->
    histo_inc(Key, float(Val));

histo_inc(Key, Val) ->
     case get_lens(Key) of
         {ok, Ptr} ->
             erl_optics_nif:histo_inc(Ptr, Val);
         {error, Msg} ->
             {error, Msg}
     end.


-spec lens_update(erl_optics_lens:t(), number()) -> ok | {error, term()}.

lens_update(Lens, Val) ->
    erl_optics_lens:update(Lens, Val).


-spec lens_free(binary()) -> ok | {error, term()}.

lens_free(Key) ->
    {ok, Lens} = get_lens(Key),
    ok = erl_optics_nif:lens_free(Lens),
    foil:delete(?NS, Key),
    ok = foil:load(?NS).


-spec quantile_update(binary(), number()) -> ok | {error, term()}.

quantile_update(Key, Val) when is_integer(Val) ->
    quantile_update(Key, float(Val));

quantile_update(Key, Val) ->
    case get_lens(Key) of
        {ok, Ptr} ->
            erl_optics_nif:quantile_update(Ptr, Val);
        {error, Msg} ->
            {error, Msg}
    end.


-spec quantile_update_timing_now(binary(), erlang:timestamp()) -> ok | {error, term()}.

quantile_update_timing_now(Key, Stamp) ->
    Delta = timer:now_diff(os:timestamp(), Stamp) / 1000.0,
    quantile_update(Key, Delta).


-spec quantile_update_timing_now_us(binary(), erlang:timestamp()) -> ok | {error, term()}.

quantile_update_timing_now_us(Key, Stamp) ->
    Delta = timer:now_diff(os:timestamp(), Stamp),
    quantile_update(Key, Delta).

-spec start(binary(), [erl_optics_lens:desc()]) -> ok.

start(Prefix, Lenses) ->
    case create_foil() of
        ok ->
            ok = create_optics(Prefix),
            ok = alloc_lenses(Lenses);
        Err -> Err
    end.


-spec start_optics(binary(), list()) -> ok | {error, term()}.

start_optics(Prefix, Lenses) ->
    %todo: check lenses validity (return failed lenses?)
    start(Prefix, Lenses),
    Poller = #{id => erl_optics_server,
               start => {erl_optics_server, start_link, []},
               shutdown => 2000,
               restart => permanent,
               type => worker,
               modules => [erl_optics_server]},
    supervisor:start_child(erl_optics_sup, Poller).


-spec stop() -> ok.

stop() ->
    {ok, Ptr} = get_optics(),
    ok = erl_optics_nif:optics_free(Ptr),
    ok = foil:delete(?NS).


-spec poll() -> ok | {ok, map()} | {error, term()}.

poll() ->
    case get_optics() of
        {ok, Ptr} ->
            erl_optics_nif:optics_poll(Ptr);
        Err -> Err
    end.

-spec register_carbon_poller(list(), non_neg_integer()) -> ok | {error, term()}.

register_carbon_poller(Host, _) when not is_list(Host) ->
    {error, "host_must_be_a_string"};

register_carbon_poller(_, Port) when not is_integer(Port) orelse Port > 65535 orelse Port < 0 ->
    {error, "invalid_port_number"};

register_carbon_poller(Host, Port) ->
    case inet:getaddr(Host, inet) of
        {ok, _Addr} ->
            case get_optics() of
                {ok, Ptr} ->
                    erl_optics_nif:register_carbon_poller(Ptr, Host, integer_to_list(Port));
                Err -> Err
            end;
        Err -> Err
    end.

-spec register_erlang_poller() -> ok | {error, term()}.

register_erlang_poller() ->
    case get_optics() of
        {ok, Ptr} ->
            erl_optics_nif:register_erlang_poller(Ptr);
        Err -> Err
    end.

%% private

alloc_lenses([]) ->
    ok = foil:load(?NS);

alloc_lenses([Lens | Rest]) ->
    Name = erl_optics_lens:name(Lens),
    {ok, Ptr} = case erl_optics_lens:type(Lens) of
        counter ->
            counter_alloc(Name);
        dist ->
            dist_alloc(Name);
        gauge ->
            gauge_alloc(Name);
        histo ->
            Buckets = erl_optics_lens:ext(Lens),
            histo_alloc(Name, Buckets);
        quantile ->
            AdjVal = erl_optics_lens:quantile_adjustment_value(Lens),
            Estimate = erl_optics_lens:quantile_estimate(Lens),
            Target = erl_optics_lens:quantile_target(Lens),
            quantile_alloc(Name, Target, Estimate, AdjVal)
    end,
    foil:insert(?NS, Name, Ptr),
    alloc_lenses(Rest).


counter_alloc(Name) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:counter_alloc(Optics, Name).


dist_alloc(Name) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:dist_alloc(Optics, Name).


gauge_alloc(Name) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:gauge_alloc(Optics, Name).


histo_alloc(Name, Buckets) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:histo_alloc(Optics, Name, Buckets).


quantile_alloc(Name, Target, Estimate, AdjVal) ->
    {ok, Optics} = get_optics(),
    erl_optics_nif:quantile_alloc(Optics, Name, Target, Estimate, AdjVal).


create_foil() ->
    case foil:new(?MODULE) of
        {error, module_exists} ->
            {error, already_started};
        ok -> ok
    end.


create_optics(Name) ->
    OpticsStatus = erl_optics_nif:optics_create(Name),
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
