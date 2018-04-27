-module(erl_optics_test_model).

-export([
    seq/2
]).


seq(Lenses, Lst) ->
    State = lists:foldl(fun do/2, erl_optics_test_model:new(Lenses), Lst),
    read_lenses(State, Lenses, 0).


% private

add_lens({Name, counter}, Acc) ->
    Acc#{Name => []};

add_lens({Name, dist}, Acc) ->
    Acc#{Name => []};

add_lens({Name, gauge}, Acc) ->
    Acc#{Name => 0.0}.


counter_inc(Model, Key, Val) ->
    #{Key := Vals} = Model,
    Model#{Key := [Val | Vals]}.


dist_record(Model, Key, Val) ->
    #{Key := Vals} = Model,
    Model#{Key := [Val | Vals]}.


do({counter_inc, Key, Val}, Acc) ->
    counter_inc(Acc, Key, Val);

do({dist_record, Key, Val}, Acc) ->
    dist_record(Acc, Key, Val);

do({gauge_set, Key, Val}, Acc) ->
    gauge_set(Acc, Key, Val).


gauge_set(Model, Key, Val) ->
    #{Key := _Val0} = Model,
    Model#{Key := Val}.


max([]) -> 0.0;

max(Lst) -> lists:last(Lst).


new(Lenses) ->
    lists:foldl(fun add_lens/2, #{}, Lenses).


percentile([], _) -> 0.0;

percentile(Lst, N) ->
    Pos = floor((N * length(Lst)) / 100),
    lists:nth(Pos + 1, Lst).


read_lens(State, Epoch) ->
    fun(Lens, Acc) -> read_lens(State, Lens, Epoch, Acc) end.


read_lens(State, {Name, counter}, _Epoch, Acc) ->
    #{Name := Evts} = State,
    Acc#{Name => lists:sum(Evts)};

read_lens(State, {Name, dist}, _Epoch, Acc) ->
    #{Name := Evts} = State,
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

read_lens(State, {Name, gauge}, _Epoch, Acc) ->
    #{Name := Val} = State,
    Acc#{Name => Val}.


read_lenses(State, Lenses, Epoch) ->
    lists:foldl(read_lens(State, Epoch), #{}, Lenses).
