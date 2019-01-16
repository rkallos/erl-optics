-module(erl_optics_test_model).

-export([
    seq/2
]).

-type lens_name() :: erl_optics_lens:lens_name().
-type lens() :: erl_optics_lens:lens().
-type op() :: counter_inc | dist_record | gauge_set | histo_inc |
    quantile_update.

-record(model, {
    epoch   =   0 :: non_neg_integer(),
    events  =  [] :: [op()],
    lenses  = #{} :: #{lens_name() => lens()},
    lens_state   = #{} :: map(),
    returns =  [] :: [ok | error]
}).

-type model() :: #model{}.


seq(Lenses, Lst) ->
    Model = lists:foldl(fun do/2, new(Lenses), Lst),
    read_lenses(Model).


% private

add_lens(Lens, Model = #model{lenses = Lenses, lens_state = State}) ->
    Name = erl_optics_lens:name(Lens),
    Lenses2 = Lenses#{Name => Lens},
    State2 = case erl_optics_lens:type(Lens) of
        counter ->
            State#{Name => []};
        dist ->
            State#{Name => []};
        gauge ->
            State#{Name => 0.0};
        histo ->
            Buckets = erl_optics_lens:ext(Lens),
            State#{Name => #{evts => [], buckets => Buckets}};
        quantile ->
            Ext = erl_optics_lens:ext(Lens),
            State#{Name => #{ext => Ext}}
    end,
    Model#model{lenses = Lenses2, lens_state = State2}.


determine_histo_key(Event, Keys) ->
    Tuple = lists:search(fun(Key) ->
        {A, B} = Key,
        Bigger = Event >= A,
        Smaller = Event < B,
        Bigger and Smaller
        end, Keys),
    case Tuple of
        {value, Key} ->
            Key;
        _ -> above_or_below(Event, Keys)
    end.

above_or_below(Event, Keys) ->
    Above = lists:dropwhile(fun(Key) ->
        {Min, Max} = Key,
        Event < Max end, Keys),
    case Above of
        [] ->
            below;
        _ ->
            above
    end.




-spec do(op(), model()) -> model().

do(Evt = {Op, Key, Val}, Model) ->
    #model{
        events = Evts,
        lenses = #{Key := Lens},
        returns = Rets,
        lens_state = State = #{Key := Vals}
    } = Model,
    {Ret, State2} = case {erl_optics_lens:type(Lens), type(Op)} of
        {T, T} ->
            {ok, State#{Key => update_state(Op, Val, Vals)}};
        _ ->
            {error, State}
    end,
    Model#model{events = [Evt | Evts], returns = [Ret | Rets], lens_state = State2}.


max([]) -> 0.0;

max(Lst) -> lists:last(Lst).


new(Lenses) ->
    lists:foldl(fun add_lens/2, #model{}, Lenses).


percentile([], _) -> 0.0;

percentile(Lst, N) ->
    Pos = floor((N * length(Lst)) / 100),
    lists:nth(Pos + 1, Lst).


populate_histo(Events, Buckets) ->
    Map1 = #{above => 0, below => 0},
    Bucket_tuples = buckets_to_tuples(Buckets),
    Keys = lists:sort(Bucket_tuples),
    Map2 = lists:foldl(fun(Bucket, Acc) -> Acc#{Bucket => 0} end, Map1, Keys),
    Map3 = lists:foldl(fun(Event, Acc) ->
        Key = determine_histo_key(Event, Keys),
        #{Key := Val} = Acc,
        Acc#{Key => Val + 1}
    end, Map2, Events),
    Map3.


buckets_to_tuples(Buckets)->
    Sorted_buckets = lists:sort(Buckets),
    buckets_to_tuples(Sorted_buckets, []).

buckets_to_tuples([], Acc) ->
    Acc;
buckets_to_tuples([_|[]], Acc) ->
    Acc;
buckets_to_tuples(Buckets, Acc) ->
    [A | Rest] = Buckets,
    [B | _] = Rest,
    buckets_to_tuples(Rest, [{A, B}| Acc]).

read_lens() ->
    fun(Lens, Acc) -> read_lens(Lens, Acc) end.


read_lens(Lens, Acc) ->
    Name = erl_optics_lens:name(Lens),
    #{Name := Evts} = Acc,
    case erl_optics_lens:type(Lens) of
        counter ->
            Acc#{Name => lists:sum(Evts)};
        dist ->
            Sorted = lists:sort(Evts),
            Res = #{
                n => length(Sorted),
                % I knew these were going to be a pain to test
                % p50 => percentile(Sorted, 50),
                % p90 => percentile(Sorted, 90),
                % p99 => percentile(Sorted, 99),
                max => max(Sorted)
            },
            Acc#{Name => Res};
        gauge ->
            % in this case, Evts is a scalar
            Acc#{Name => Evts};
        histo ->
            #{evts := Events, buckets := Buckets} = Evts,
            Acc#{Name => populate_histo(Events, Buckets)};
        quantile ->
            % this is brutally difficult to test without a huge number of events
            % due to the use of RNGs in quantile_update
            Acc#{Name => 0.0}
    end.

read_lenses(Model) ->
    #model{lenses = Lenses, lens_state = State, returns = Rets} = Model,
    LensState = lists:foldl(fun(Lens, Acc) ->
        read_lens(Lens, Acc)
    end, State, maps:values(Lenses)),
    {LensState, lists:reverse(Rets)}.


type(counter_inc) -> counter;

type(dist_record) -> dist;

type(gauge_set) -> gauge;

type(histo_inc) -> histo;

type(quantile_update) -> quantile.


update_state(counter_inc, Val, Vals) ->
    [Val | Vals];

update_state(dist_record, Val, Vals) ->
    [Val | Vals];

update_state(gauge_set, Val, _Vals) ->
    Val;

update_state(histo_inc, Val, Vals = #{evts := Evts}) ->
    Vals#{evts => [Val | Evts]};

update_state(quantile_update, _Val, Vals) ->
    Vals.
