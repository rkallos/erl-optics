-module(prop_base).
-include_lib("proper/include/proper.hrl").

-define(OPS, [counter_inc, dist_record, gauge_set, histo_inc, quantile_update]).

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
    ErlModel =:= CModel.



%% Generators

error_event(Lenses) ->
    ?LET(Lens, oneof(Lenses), begin
        Name = erl_optics_lens:name(Lens),
        case erl_optics_lens:type(Lens) of
            counter ->
                ?LAZY(?LET([Op, Val], [oneof(?OPS -- [counter_inc]), number()],
                    {Op, Name, Val}));
            dist ->
                ?LAZY(?LET([Op, Val], [oneof(?OPS -- [dist_record]), number()],
                    {Op, Name, Val}));
            gauge ->
                ?LAZY(?LET([Op, Val], [oneof(?OPS -- [gauge_set]), number()],
                    {Op, Name, Val}));
            histo ->
                ?LAZY(?LET([Op, Val], [oneof(?OPS -- [histo_inc]), number()],
                    {Op, Name, Val}));
            quantile ->
                ?LAZY(?LET([Op, Val], [oneof(?OPS -- [quantile_update]), number()],
                    {Op, Name, Val}))
        end
    end).

event(Lenses) ->
    weighted_union([
        {90, ok_event(Lenses)},
        {10, error_event(Lenses)}
    ]).


histo_buckets(Len) -> vector(Len, non_neg_integer()).


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
                    erl_optics_lens:histo(Name, lists:usort(Buckets))));
            quantile ->
                ?LAZY(?LET([T, E, A], [non_neg_float(), non_neg_float(), non_neg_float()],
                    erl_optics_lens:quantile(Name, T, E, A)))
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


ok_event(Lenses) ->
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
                    {histo_inc, Name, Val}));
            quantile ->
                ?LAZY(?LET([Val], [non_neg_float()],
                    {quantile_update, Name, Val}))
        end
    end).


seq() ->
    ?LET(Lenses, non_empty(list(lens())), begin
        Map = lists:foldl(fun(Lens, Acc) ->
            Acc#{erl_optics_lens:name(Lens) => Lens}
        end, #{}, Lenses),
        UniqueLenses = maps:values(Map),
        {UniqueLenses, non_empty(list(event(maps:values(Map))))}
    end).
