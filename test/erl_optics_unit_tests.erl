-module(erl_optics_unit_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    start(),
    check_in_foil(optics),
    stop(),
    ?assertEqual({error, module_not_found}, foil:lookup(erl_optics, optics)).

counter_inc_test() ->
    start(),
    erl_optics:counter_inc(<<"counter">>),
    foil_reload(),
    check_in_foil(<<"counter">>),
    erl_optics:counter_inc(<<"counter">>),
    Val = poll_for_key(<<"counter">>),
    ?assertEqual(Val, 2),
    stop().

dist_record_test() ->
    start(),
    erl_optics:dist_record(<<"dist">>, 1.0),
    foil_reload(),
    check_in_foil(<<"dist">>),
    compare_dist(poll_for_key(<<"dist">>),
        1.0, 1, 1.0, 1.0, 1.0, 0.0),
    dist_record_n_times(<<"dist">>, 1.0, 60),
    dist_record_n_times(<<"dist">>, 2.0, 39),
    dist_record_n_times(<<"dist">>, 3.0, 1),
    dist_record_n_times(<<"dist">>, 10.0, 1),
    compare_dist(poll_for_key(<<"dist">>),
        10.0, 101, 1.0, 2.0, 3.0, 0.0),
    dist_record_ramp(<<"dist">>, 200),
    compare_dist(poll_for_key(<<"dist">>),
        200.0, 200, 100, 180, 198, 1.0),
    stop().

gauge_set_test() ->
    start(),
    erl_optics:gauge_set(<<"gauge">>, 1.0),
    foil_reload(),
    check_in_foil(<<"gauge">>),
    ?assertEqual(poll_for_key(<<"gauge">>), 1.0),
    erl_optics:gauge_set(<<"gauge">>, 1.0),
    erl_optics:gauge_set(<<"gauge">>, 2.0),
    ?assertEqual(poll_for_key(<<"gauge">>), 2.0),
    stop().

histo_inc_test() ->
    start([erl_optics_lens:histo(<<"histo">>, [0, 10, 20, 30, 40])]),

    erl_optics:histo_inc(<<"histo">>, -5),
    WantMapBelow = #{above => 0, below => 1,
                {0, 10} => 0,
                {10, 20} => 0,
                {20, 30} => 0,
                {30, 40} => 0},
    ?assertEqual(poll_for_key(<<"histo">>), WantMapBelow),

    erl_optics:histo_inc(<<"histo">>, 50),
    WantMapAbove = #{above => 1, below => 0,
                {0, 10} => 0,
                {10, 20} => 0,
                {20, 30} => 0,
                {30, 40} => 0},
    ?assertEqual(poll_for_key(<<"histo">>), WantMapAbove),

    histo_inc_ramp(<<"histo">>, 50),
    WantMapRamp = #{above => 10, below => 0,
                     {0, 10} => 10,
                     {10, 20} => 10,
                     {20, 30} => 10,
                     {30, 40} => 10},
    ?assertEqual(poll_for_key(<<"histo">>), WantMapRamp),
    stop().

quantile_update_test() ->
    ok.

lens_free_test() ->
    ok.

%%% utils

start() ->
    start([]).

start(Lenses) ->
    erl_optics_app:start(),
    erl_optics:start_optics(<<"test">>, Lenses),
    erl_optics:register_erlang_poller().

foil_reload() ->
    Pid = whereis(erl_optics_foil_server),
    gen_server:call(Pid, reload_foil).

stop() ->
    erl_optics:stop().

check_in_foil(Key) ->
    {ok, _Ptr} = foil:lookup(erl_optics, Key).

dist_record_n_times(_Key, _Val, 0) ->
    ok;
dist_record_n_times(Key, Val, N) ->
    erl_optics:dist_record(Key, Val),
    dist_record_n_times(Key, Val, N - 1).

dist_record_ramp(_Key, 0) ->
    ok;
dist_record_ramp(Key, N) ->
    erl_optics:dist_record(Key, N),
    dist_record_ramp(Key, N - 1).

histo_inc_ramp(_Key, 0) ->
    ok;
histo_inc_ramp(Key, N) ->
    erl_optics:histo_inc(Key, N - 1),
    histo_inc_ramp(Key, N - 1).




compare_dist(Dist, Max, N, P50, P90, P99, Epsilon) ->
    ?assertEqual(Max, maps:get(max, Dist)),
    ?assertEqual(N, maps:get(n, Dist)),
    ?assertEqual(P50, compare_with_error(P50, maps:get(p50, Dist), Epsilon)),
    ?assertEqual(P90, compare_with_error(P90, maps:get(p90, Dist), Epsilon)),
    ?assertEqual(P99, compare_with_error(P99, maps:get(p99, Dist), Epsilon)).

compare_with_error(A, B, Epsilon) ->
    case abs(A - B) =< Epsilon of
        true ->
            A;
        false ->
            B
    end.

poll_for_key(Key) ->
    {ok, PollMap} = erl_optics:poll(),
    {_Type, Val} = maps:get({<<"test">>, Key}, PollMap),
    Val.
