-module(habanero).
-author('James Kelly <jim@adroll.com>').

-export([start/0, stop/0]).

start() ->
    ok = habanero_deps:ensure(),
    ok = ensure_started(habanero).

stop() ->
    ok = application:stop(habanero).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {not_started, DepApp}} ->
            ensure_started(DepApp),
            ensure_started(App);
        {error, {already_started, App}} ->
            ok
    end.
