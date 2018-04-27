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
    %io:format("~p~n", [#{erl => ErlModel, c => CModel, seq => Seq, lenses => Lenses}]),
    ErlModel =:= CModel.


%% Generators

event(Lenses) ->
    ?LET(Lens, oneof(Lenses),
        case Lens of
            {Name, counter} ->
                {counter_inc, Name, pos_integer()};
            {Name, dist} ->
                {dist_record, Name, float(0.0, inf)};
            {Name, gauge} ->
                {gauge_set, Name, float()}
        end).


lens() ->
    ?LET([Name, Type], [lens_name(), type()], {Name, Type}).

lens_name() ->
    UA = $A,
    UZ = $Z,
    DA = $a,
    DZ = $z,
    UChar = integer(UA, UZ),
    DChar = integer(DA, DZ),
    ?LET(Str, non_empty(list(oneof([UChar, DChar]))), list_to_binary(Str)).


seq() ->
    ?LET(Lenses, non_empty(list(lens())), begin
        UniqueLenses = maps:to_list(maps:from_list(Lenses)),
        {UniqueLenses, non_empty(list(event(Lenses)))}
    end).


type() -> oneof([counter, dist, gauge]).
