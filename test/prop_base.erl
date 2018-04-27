-module(prop_base).
-include_lib("proper/include/proper.hrl").

%% properties

prop_test() ->
    ?FORALL(Seq, seq(),
        begin
            check(Seq)
        end).


%% helpers

check({Lenses, Seq}) ->
    ErlModel = erl_optics_test_model:seq(Lenses, Seq),
    CModel = erl_optics_test_utils:seq(Lenses, Seq),
    %io:format("~p~n", [#{erl => ErlModel, c => CModel}]),
    ErlModel =:= CModel.


%% Generators

counter_inc() ->
    {counter_inc, <<"counter">>, pos_integer()}.


dist_record() ->
    {dist_record, <<"dist">>, float(0.0, inf)}.


gauge_set() ->
    {gauge_set, <<"gauge">>, float()}.


seq() ->
    {lenses(), list(oneof([
        dist_record(),
        counter_inc(),
        gauge_set()
    ]))}.


lenses() -> [
    erl_optics_lens:counter(<<"counter">>),
    erl_optics_lens:gauge(<<"gauge">>),
    erl_optics_lens:dist(<<"dist">>)
].
