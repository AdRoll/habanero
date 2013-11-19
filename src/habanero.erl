-module(habanero).
-author('James Kelly <jim@adroll.com>').

-export([start/0, stop/0]).

start() ->
    habanero_deps:ensure(),
    _ = application:start(lager),
    _ = application:start(folsom),
    _ = application:start(ranch),
    _ = application:start(cowboy),
    _ = application:start(erlang_js),
    _ = application:start(habanero).

stop() ->
    _ = application:stop(habanero).
