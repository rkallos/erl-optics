-module(erl_optics).

-compile(no_native).
-on_load(on_load/0).

-define(NS, ?MODULE).

-export([counter_inc/1,
         counter_inc/2,
         dist_record/2,
         gauge_set/2,
         lens_free/1,
         start/0,
         start/1,
         stop/0]).

-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key, Amt) ->
    {ok, Ptr} = get_lens(Key),
    counter_inc_nif(Ptr, Amt).


-spec dist_record(binary(), float()) -> ok | {error, term()}.

dist_record(Key, Val) ->
    dist_record_nif(Key, Val).


-spec gauge_set(binary(), number()) -> ok | {error, term()}.

gauge_set(Key, Val) when is_integer(Val) ->
    gauge_set_nif(Key, float(Val));

gauge_set(Key, Val) ->
    gauge_set_nif(Key, Val).


-spec lens_free(binary()) -> ok | {error, term()}.

lens_free(Key) ->
    lens_free_nif(Key).


-spec start() -> ok.

start() ->
    Lenses = [erl_optics_lens:counter(<<"bob_the_counter">>),
              erl_optics_lens:gauge(<<"bob_the_gauge">>),
              erl_optics_lens:dist(<<"bob_the_dist">>)],
    start(Lenses).

-spec start([erl_optics_lens:desc()]) -> ok.

start(Lenses) ->
    case create_foil() of
        ok -> create_optics(Lenses);
        Err -> Err
    end.

-spec stop() -> ok.

stop() ->
    {ok, Ptr} = get_optics(),
    ok = optics_free_nif(Ptr),
    ok = foil:delete(?NS).

%% private

alloc_lenses([]) ->
    foil:load(?NS);

alloc_lenses([{Name, Type} | Rest]) ->
    {ok, Ptr} = case Type of
        counter -> alloc_counter(Name);
           dist -> alloc_dist(Name);
          gauge -> alloc_gauge(Name)
    end,
    foil:insert(?NS, Name, Ptr),
    alloc_lenses(Rest).


alloc_counter(Name) ->
    {ok, Optics} = get_optics(),
    alloc_counter_nif(Optics, Name).


alloc_dist(Name) ->
    {ok, Optics} = get_optics(),
    alloc_dist_nif(Optics, Name).


alloc_gauge(Name) ->
    {ok, Optics} = get_optics(),
    alloc_gauge_nif(Optics, Name).


create_foil() ->
    application:ensure_all_started(foil),
    case foil:new(?MODULE) of
        {error, module_exists} ->
            {error, already_started};
        ok -> ok
    end.

create_optics(Lenses) ->
    OpticsStatus = optics_alloc_nif(),
    case OpticsStatus of
        {ok, Ptr} ->
            foil:insert(?MODULE, optics, Ptr),
            foil:load(?MODULE),
            alloc_lenses(Lenses);
        _ ->
            {error, cannot_alloc_optics}
    end.


get_lens(Key) ->
    foil:lookup(?NS, Key).


get_optics() ->
    foil:lookup(?NS, optics).


-spec on_load() -> ok.

on_load() ->
    SoName = case code:priv_dir(erl_optics) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, erl_optics]);
                _ ->
                    filename:join([priv, erl_optics])
            end;
        Dir ->
            filename:join(Dir, erl_optics)
    end,
    ok = erlang:load_nif(SoName, 0).


%% nifs

%% shamelessly stolen from crypto.erl
-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

alloc_counter_nif(_Optics, _Name) ->
    ?nif_stub.
alloc_dist_nif(_Optics, _Name) ->
    ?nif_stub.
alloc_gauge_nif(_Optics, _Name) ->
    ?nif_stub.
counter_inc_nif(_K, _V) ->
    ?nif_stub.
dist_record_nif(_K, _V) ->
    ?nif_stub.
gauge_set_nif(_K, _V) ->
    ?nif_stub.
lens_free_nif(_K) ->
    ?nif_stub.
optics_alloc_nif() ->
    ?nif_stub.
optics_free_nif(_Ptr) ->
    ?nif_stub.
