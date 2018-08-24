-module(erl_optics_lens).

-compile(inline).

-export([
    counter/1,
    dist/1,
    ext/1,
    gauge/1,
    histo/2,
    name/1,
    quantile/4,
    quantile_adjustment_value/1,
    quantile_estimate/1,
    quantile_target/1,
    type/1,
    update/2
]).

-type histo_buckets() :: list(float()).

-record(quantile_args, {
    adjustment_value = undefined :: float(),
    estimate = undefined :: float(),
    target = undefined :: float()
}).

-opaque quantile_args() :: #quantile_args{}.

-type lens_name() :: bitstring().
-type lens_type() :: counter | dist | gauge | histo | quantile.
-type lens_ext() :: histo_buckets() | quantile_args() | undefined.

-record(lens, {
    name = undefined :: lens_name(),
    f = undefined :: fun((number()) -> ok | {error, term()}),
    type = undefined :: lens_type(),
    ext = undefined :: lens_ext() | undefined
}).

-opaque lens() :: #lens{}.

-export_type([
    histo_buckets/0,
    lens/0,
    lens_name/0,
    lens_type/0,
    lens_ext/0,
    quantile_args/0
]).


-spec counter(lens_name()) -> lens().

counter(Name) when is_binary(Name) ->
    Fun = fun(Val) -> erl_optics:counter_inc(Name, Val) end,
    #lens{name = Name, type = counter, f = Fun}.


-spec dist(lens_name()) -> lens().

dist(Name) when is_binary(Name) ->
    Fun = fun(Val) -> erl_optics:dist_record(Name, Val) end,
    #lens{name = Name, type = dist, f = Fun}.


-spec ext(lens()) -> lens_ext().

ext(#lens{ext = Ext}) -> Ext.


-spec gauge(lens_name()) -> lens().

gauge(Name) when is_binary(Name) ->
    Fun = fun(Val) -> erl_optics:gauge_set(Name, Val) end,
    #lens{name = Name, type = gauge, f = Fun}.


-spec histo(lens_name(), list(float())) -> lens().

histo(Name, Buckets) when is_binary(Name) ->
    Fun = fun(Val) -> erl_optics:histo_inc(Name, Val) end,
    #lens{name = Name, type = histo, f = Fun, ext = Buckets}.


-spec name(lens()) -> binary().

name(#lens{name = Name}) -> Name.


-spec quantile(lens_name(), float(), float(), float()) -> lens().

quantile(Name, Target, Estimate, AdjVal) ->
    Ext = #quantile_args{
        adjustment_value = AdjVal,
        estimate = Estimate,
        target = Target
    },
    Fun = fun(Val) -> erl_optics:quantile_update(Name, Val) end,
    #lens{name = Name, type = quantile, f = Fun, ext = Ext}.


-spec quantile_adjustment_value(lens()) -> float().

quantile_adjustment_value(
    #lens{
        type = quantile,
        ext = #quantile_args{
            adjustment_value = V
        }
    }) -> V.


-spec quantile_estimate(lens()) -> float().

quantile_estimate(
    #lens{
        type = quantile,
        ext = #quantile_args{
            estimate = V
        }
    }) -> V.


-spec quantile_target(lens()) -> float().

quantile_target(
    #lens{
        type = quantile,
        ext = #quantile_args{
            target = V
        }
    }) -> V.


-spec type(lens()) -> lens_type().

type(#lens{type = Type}) -> Type.


-spec update(lens(), number()) -> ok | {error, term()}.

update(#lens{f = Fun}, Val) ->
    Fun(Val).
