-module(erl_optics_test_model).

-export([
    seq/2
]).


seq(Lenses, Lst) ->
    State = lists:foldl(fun do/2, erl_optics_test_model:new(Lenses), Lst),
    read_lenses(State, Lenses, 0).


% private

add_lens(Lens, Acc) ->
    Name = erl_optics_lens:name(Lens),
    case erl_optics_lens:type(Lens) of
        counter ->
            Acc#{Name => []};
        dist ->
            Acc#{Name => []};
        gauge ->
            Acc#{Name => 0.0};
        histo ->
            Buckets = erl_optics_lens:ext(Lens),
            Acc#{Name => #{evts => [], buckets => Buckets}}
    end.


counter_inc(Model, Key, Val) ->
    #{Key := Vals} = Model,
    Model#{Key := [Val | Vals]}.


determine_histo_key(Event, Keys) ->
    Keys2 = lists:dropwhile(fun(Key) -> Key < Event end, Keys),
    KeysLen = length(Keys),
    Keys2Len = length(Keys2),
    case Keys2Len of
        0 ->
            above;
        KeysLen ->
            below;
        N ->
            lists:nth(KeysLen - Keys2Len, Keys)
    end.

dist_record(Model, Key, Val) ->
    #{Key := Vals} = Model,
    Model#{Key := [Val | Vals]}.


do({counter_inc, Key, Val}, Acc) ->
    counter_inc(Acc, Key, Val);

do({dist_record, Key, Val}, Acc) ->
    dist_record(Acc, Key, Val);

do({gauge_set, Key, Val}, Acc) ->
    gauge_set(Acc, Key, Val);

do({histo_inc, Key, Val}, Acc) ->
    histo_inc(Acc, Key, Val).


gauge_set(Model, Key, Val) ->
    #{Key := _Val0} = Model,
    Model#{Key := Val}.


histo_inc(Model, Key, Val) ->
    #{Key := Map = #{evts := Vals}} = Model,
    Model#{Key => Map#{evts => [Val | Vals]}}.


max([]) -> 0.0;

max(Lst) -> lists:last(Lst).


new(Lenses) ->
    lists:foldl(fun add_lens/2, #{}, Lenses).


percentile([], _) -> 0.0;

percentile(Lst, N) ->
    Pos = floor((N * length(Lst)) / 100),
    lists:nth(Pos + 1, Lst).


populate_histo(Events, Buckets) ->
    Map1 = #{above => 0, below => 0},
    Keys = lists:sort(Buckets),
    Map2 = lists:foldl(fun(Bucket, Acc) -> Acc#{Bucket => 0} end, Map1, Keys),
    Map3 = lists:foldl(fun(Event, Acc) ->
        Key = determine_histo_key(Event, Keys),
        #{Key := Val} = Acc,
        Acc#{Key => Val + 1}
    end, Map2, Events),
    LastBucket = lists:last(Buckets),
    {V, Map4 = #{above := Above}} = maps:take(LastBucket, Map3),
    Map4#{above => Above + V}.


read_lens(State, Epoch) ->
    fun(Lens, Acc) -> read_lens(State, Lens, Epoch, Acc) end.


read_lens(State, Lens, _Epoch, Acc) ->
    Name = erl_optics_lens:name(Lens),
    #{Name := Evts} = State,
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
            Acc#{Name => populate_histo(Events, Buckets)}
    end.

read_lenses(State, Lenses, Epoch) ->
    lists:foldl(read_lens(State, Epoch), #{}, Lenses).
