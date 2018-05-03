-module(erl_optics_test_utils).

-export([
    seq/2
]).

seq(Lenses, Lst) ->
    erl_optics:start(Lenses),
    Returns = lists:map(fun(Evt) ->
        case do(Evt) of
            ok -> ok;
            {error, _} -> error
        end
    end, Lst),
    State = read_lenses(Lenses, 0),
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
    erl_optics:histo_inc(Key, Val).


read_lenses(Lenses, Epoch) ->
    lists:foldl(read_lens(Epoch), #{}, Lenses).


read_lens(Epoch) ->
    fun(Lens, Acc) -> read_lens(Lens, Epoch, Acc) end.


read_lens(Lens, Epoch, Acc) ->
    Name = erl_optics_lens:name(Lens),
    {ok, Ptr} = erl_optics:get_lens(Name),
    case erl_optics_lens:type(Lens) of
        counter ->
            Acc#{Name => erl_optics_nif:counter_read(Ptr, Epoch)};
        dist ->
            Map0 = erl_optics_nif:dist_read(Ptr, Epoch),
            % Given optics's PRNG-based reservoir eviction, .n and .max % are really the
            % only two values that can be tested deterministically
            Map = maps:with([n, max], Map0),
            Acc#{Name => Map};
        gauge ->
            Acc#{Name => erl_optics_nif:gauge_read(Ptr, Epoch)};
        histo ->
            Acc#{Name => erl_optics_nif:histo_read(Ptr, Epoch)}
    end.
