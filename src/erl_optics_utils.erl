-module(erl_optics_utils).

-export([multiple_counter_lens/2, multiple_gauge_lens/2, multiple_dist_lens/2, multiple_quantile_lens/5, multiple_histo_lens/3]).

-export([quantile_name_generator/3]).


multiple_counter_lens_in(Key, 0, Lst)->
    Lst;

multiple_counter_lens_in(Key, N, Lst)->
    multiple_counter_lens_in(Key, N-1, [erl_optics_lens:counter(default_generator(Key, N))|Lst]).

multiple_counter_lens(Key, N)->
    multiple_counter_lens_in(Key, N, []).
                                          
    
default_generator(Key, N)->
    list_to_binary([Key, $., integer_to_binary(N)]).


multiple_gauge_lens_in(Key, 0, Lst)->
    Lst;
multiple_gauge_lens_in(Key, N, Lst) ->
    multiple_gauge_lens_in(Key, N-1, [erl_optics_lens:gauge(default_generator(Key, N))|Lst]).

multiple_gauge_lens(Key, N) ->
    multiple_gauge_lens_in(Key, N, []).


multiple_dist_lens_in(Key, 0, Lst)->
    Lst;
multiple_dist_lens_in(Key, N, Lst) ->
    multiple_dist_lens_in(Key, N-1, [erl_optics_lens:dist(default_generator(Key, N))|Lst]).

multiple_dist_lens(Key, N)->
    multiple_dist_lens_in(Key, N, []).



%% multiple_quantile_lens_in(Key, Target, Estimate, AdjVal, 0, Lst)->
%%     Lst;
%% multiple_quantile_lens_in(Key, Target, Estimate, AdjVal, N, Lst) when not is_list(Target)->
%%     multiple_quantile_lens_in(Key, [Target], Estimate, AdjVal, N, Lst);
%% multiple_quantile_lens_in(Key, Target, Estimate, AdjVal,  N, Lst) ->
%%     multiple_quantile_lens_in(Key,  Target, Estimate, AdjVal, N-1, [erl_optics_lens:quantile(default_generator(Key, N), Target, Estimate, AdjVal)|Lst]).

%% multiple_quantile_lens(Key,  Target, Estimate, AdjVal, N)->
%%     multiple_quantile_lens_in(Key,  Target, Estimate, AdjVal, N, []).


multiple_quantile_lens(Key, TargetLst, Estimate, AdjVal, N)->
    [erl_optics_lens:quantile(quantile_name_generator(Key, Nb, Target), Target, Estimate, AdjVal) || Nb <- lists:seq(1, N), Target <- TargetLst].

quantile_name_generator(Key, Nb, Target)->
    list_to_binary([Key, $., integer_to_binary(Nb), $., $q, list_to_binary(binary:split(float_to_binary(Target, [{decimals, 2}]), <<".">>))]).


multiple_histo_lens_in(Key, Buckets, 0, Lst)-> 
    Lst;
multiple_histo_lens_in(Key, Buckets, N, Lst)-> 
    multiple_histo_lens_in(Key, Buckets, N-1, [erl_optics_lens:histo(default_generator(Key, N), Buckets)|Lst]).

multiple_histo_lens(Key, Buckets, N)->
    multiple_histo_lens_in(Key, Buckets, N, []).
