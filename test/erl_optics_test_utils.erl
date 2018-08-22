-module(erl_optics_test_utils).

-export([
    seq/2
]).

seq(Lenses, Lst) ->
    erl_optics:start(Lenses),
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
    lists:foldl(fun(Lens, Acc) ->
        read_lens(Lens, Acc)
    end, #{}, Lenses).


read_lens(Lens, Acc) ->
    Name = erl_optics_lens:name(Lens),
    {ok, Ptr} = erl_optics:get_lens(Name),
    case erl_optics_lens:type(Lens) of
        counter ->
            Acc#{Name => erl_optics_nif:counter_read(Ptr)};
        dist ->
            Map0 = erl_optics_nif:dist_read(Ptr),
            % Given optics's PRNG-based reservoir eviction, .n and .max % are really the
            % only two values that can be tested deterministically
            Map = maps:with([n, max], Map0),
            Acc#{Name => Map};
        gauge ->
            Acc#{Name => erl_optics_nif:gauge_read(Ptr)};
        histo ->
            Acc#{Name => erl_optics_nif:histo_read(Ptr)};
        quantile ->
            % Can't check in Erlang due to PRNG-based updating
            _Val = erl_optics_nif:quantile_read(Ptr),
            Acc#{Name => 0.0}
    end.
