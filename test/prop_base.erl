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
    ?LET(Lens, oneof(Lenses), begin
        Name = erl_optics_lens:name(Lens),
        case erl_optics_lens:type(Lens) of
            counter ->
                ?LAZY(?LET([Val], [pos_integer()],
                    {counter_inc, Name, Val}));
            dist ->
                ?LAZY(?LET([Val], [non_neg_float()],
                    {dist_record, Name, Val}));
            gauge ->
                ?LAZY(?LET([Val], [float()],
                    {gauge_set, Name, Val}));
            histo ->
                ?LAZY(?LET([Val], [non_neg_float()],
                    {histo_inc, Name, Val}))
        end
    end).


histo_buckets(Len) -> vector(Len, non_neg_float()).


lens() ->
    ?LET([Name, Type], [lens_name(), erl_optics_lens:lens_type()], begin
        case Type of
            counter ->
                erl_optics_lens:counter(Name);
            dist ->
                erl_optics_lens:dist(Name);
            gauge ->
                erl_optics_lens:gauge(Name);
            histo ->
                ?LAZY(?LET([Buckets], [histo_buckets(8)],
                    erl_optics_lens:histo(Name, lists:usort(Buckets))))
        end
    end).


% because using erl_optics_lens:lens_name() is not restrictive enough
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
        Map = lists:foldl(fun(Lens, Acc) ->
            Acc#{erl_optics_lens:name(Lens) => Lens}
        end, #{}, Lenses),
        UniqueLenses = maps:values(Map),
        {UniqueLenses, non_empty(list(event(Lenses)))}
    end).