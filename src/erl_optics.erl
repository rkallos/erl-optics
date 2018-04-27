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

-export([
    counter_read_nif/2,
    dist_read_nif/2,
    gauge_read_nif/2,
    optics_epoch_nif/1
]).

-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key, Amt) ->
    {ok, Ptr} = get_lens(Key),
    counter_inc_nif(Ptr, Amt).


-spec dist_record(binary(), float()) -> ok | {error, term()}.

dist_record(Key, Val) ->
    {ok, Ptr} = get_lens(Key),
    dist_record_nif(Ptr, Val).


-spec gauge_set(binary(), number()) -> ok | {error, term()}.

gauge_set(Key, Val) when is_integer(Val) ->
    gauge_set(Key, float(Val));

gauge_set(Key, Val) ->
    {ok, Ptr} = get_lens(Key),
    gauge_set_nif(Ptr, Val).


-spec lens_free(binary()) -> ok | {error, term()}.

lens_free(Key) ->
    {ok, Lens} = get_lens(Key),
    ok = lens_free_nif(Lens),
    foil:delete(?NS, Key),
    foil:load(?NS).


-spec start() -> ok.

start() ->
    Lenses = [erl_optics_lens:counter(<<"bob_the_counter">>),
              erl_optics_lens:gauge(<<"bob_the_gauge">>),
              erl_optics_lens:dist(<<"bob_the_dist">>)],
    start(Lenses).

-spec start([erl_optics_lens:desc()]) -> ok.

start(Lenses) ->
    case create_foil() of
        ok ->
            ok = create_optics(),
            ok = alloc_lenses(Lenses);
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

create_optics() ->
    OpticsStatus = optics_alloc_nif(),
    case OpticsStatus of
        {ok, Ptr} ->
            ok = foil:insert(?MODULE, optics, Ptr),
            ok = foil:load(?MODULE);
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
optics_epoch_nif(_Ptr) ->
    ?nif_stub.
optics_free_nif(_Ptr) ->
    ?nif_stub.
counter_read_nif(_Lens, _Epoch) ->
    ?nif_stub.
dist_read_nif(_Lens, _Epoch) ->
    ?nif_stub.
gauge_read_nif(_Lens, _Epoch) ->
    ?nif_stub.
