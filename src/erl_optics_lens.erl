-module(erl_optics_lens).

-compile(inline).

-export([
    counter/1,
    dist/1,
    ext/1,
    gauge/1,
    histo/2,
    name/1,
    type/1
]).

-type histo_buckets() :: list(float()).

-type lens_name() :: bitstring().
-type lens_type() :: counter | dist | gauge | histo.
-type lens_ext() :: histo_buckets() | undefined.

-record(lens, {
    name = undefined :: lens_name(),
    type = undefined :: lens_type(),
    ext = undefined :: lens_ext() | undefined
}).

-opaque lens() :: #lens{}.

-export_type([
    histo_buckets/0,
    lens/0,
    lens_name/0,
    lens_type/0,
    lens_ext/0
]).


-spec counter(lens_name()) -> lens().

counter(Name) when is_binary(Name) ->
    #lens{name = Name, type = counter}.


-spec dist(lens_name()) -> lens().

dist(Name) when is_binary(Name) ->
    #lens{name = Name, type = dist}.


-spec ext(lens()) -> lens_ext().

ext(#lens{ext = Ext}) -> Ext.


-spec gauge(lens_name()) -> lens().

gauge(Name) when is_binary(Name) ->
    #lens{name = Name, type = gauge}.


-spec histo(lens_name(), list(float())) -> lens().

histo(Name, Buckets) when is_binary(Name) ->
    #lens{name = Name, type = histo, ext = Buckets}.


-spec name(lens()) -> binary().

name(#lens{name = Name}) -> Name.


-spec type(lens()) -> lens_type().

type(#lens{type = Type}) -> Type.
