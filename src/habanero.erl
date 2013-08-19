-module(habanero).
-author('James Kelly <jim@adroll.com>').

-export([start/0, stop/0]).

start() ->
    habanero_deps:ensure(),
    application:start(lager),
    application:start(folsom),
    application:start(ranch),
    application:start(cowboy),
    application:start(erlang_js),
    application:start(habanero).

stop() ->
    application:stop(habanero).

