-module(erl_optics_test_utils).

-export([
    seq/2
]).

seq(Lenses, Lst) ->
    erl_optics:start(<<"test">>, Lenses),
    erl_optics:allocate_erlang_poller(),
    Returns = lists:map(fun(Evt) ->
        case catch do(Evt) of
            ok -> ok;
            {error, _} -> error;
            {'EXIT', {badarg, _}} -> error
        end
    end, Lst),
    State = read_lenses(Lenses),
    erl_optics:stop(),
    {State, Returns}.


% private

do({dist_record, Key, Val}) ->
    erl_optics:dist_record(Key, Val);

do({counter_inc, Key, Val}) ->
    erl_optics:counter_inc(Key, Val);

do({gauge_set, Key, Val}) ->
    erl_optics:gauge_set(Key, Val);

do({histo_inc, Key, Val}) ->
    erl_optics:histo_inc(Key, Val);

do({quantile_update, Key, Val}) ->
    erl_optics:quantile_update(Key, Val).


read_lenses(Lenses) ->
    {ok, PollMap} = erl_optics:poll(),
    lists:foldl(fun (Lens, Acc) ->
        Name = erl_optics_lens:name(Lens),
        {Type, Val} = maps:get({<<"test">>, Name}, PollMap),
        case Type of
            counter ->
                Acc#{Name => Val};
            dist ->
                Acc#{Name => #{n => maps:get(n, Val), max => maps:get(max, Val)}};
            gauge ->
                Acc#{Name => Val};
            histo ->
                Acc#{Name => Val};
            quantile ->
                Acc#{Name => 0.0}
        end
    end, #{}, Lenses).
