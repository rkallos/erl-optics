-module(erl_optics_lens).

-compile(inline).

-export([
    counter/1,
    gauge/1,
    dist/1,
    name/1,
    type/1
]).

-type lens_type() :: counter | dist | gauge.

-record(optics_lens, {
    name = undefined :: bitstring(),
    type = undefined :: lens_type()
}).

-opaque optics_lens() :: #optics_lens{}.

-export_type([optics_lens/0]).


-spec counter(binary()) -> optics_lens().

counter(Name) when is_binary(Name) ->
    #optics_lens{name = Name, type = counter}.


-spec dist(binary()) -> optics_lens().

dist(Name) when is_binary(Name) ->
    #optics_lens{name = Name, type = dist}.


-spec gauge(binary()) -> optics_lens().

gauge(Name) when is_binary(Name) ->
    #optics_lens{name = Name, type = gauge}.


-spec name(optics_lens()) -> binary().

name(#optics_lens{name = Name}) -> Name.


-spec type(optics_lens()) -> lens_type().

type(#optics_lens{type = Type}) -> Type.
