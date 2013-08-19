-module(habanero_sup).
-author('James Kelly <jim@adroll.com>').

-behaviour(supervisor).

-include("habanero.hrl").

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, [?MODULE_ENV(I)]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(habanero_coordinator, worker),
        ?CHILD(habanero_line_server, worker)
    ]}}.

