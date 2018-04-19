-module(erl_optics_lens).

-compile(inline).

-type lens_type() :: counter | dist | gauge.

-type desc() :: {binary(), lens_type()}.

-export_type([desc/0]).

-export([counter/1,
         gauge/1,
         dist/1]).


-spec counter(binary()) -> desc().

counter(Name) when is_binary(Name) ->
    {Name, counter}.


-spec dist(binary()) -> desc().

dist(Name) when is_binary(Name) ->
    {Name, dist}.


-spec gauge(binary()) -> desc().

gauge(Name) when is_binary(Name) ->
    {Name, gauge}.
