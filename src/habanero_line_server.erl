%% @doc Loads data files into memory.
%%
%% Multiple data files can be defined in the habanero_line_server configuration, for example:
%%
%%          {habanero_line_server, [
%%                {login_data, ["priv", "data", "login_data.data"]},
%%                {form_data, ["priv", "data", "form_data.data"]}
%%          ]}
%%
%% Contents of these files can then be retrieved via:
%%
%%      habanero_line_server:random_line(login_data)
-module(habanero_line_server).
-author('James Kelly <jim@adroll.com>').

-behaviour(gen_server).

-include("habanero.hrl").

%% API.
-export([start_link/1]).
-export([random_line/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(LINE_COUNT(Type), {Type, line_count}).
-define(LINE_ITEM(Type, I), {Type, I}).

-type path_list() :: list(string()).
-type line_server_definition() :: list({atom, path_list()}).

-spec start_link(line_server_definition()) -> ok.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec random_line(atom()) -> string().
random_line(Type) ->
    case ets:lookup(line_server, ?LINE_COUNT(Type)) of
        [] ->
            undefined;
        [{_, N}] ->
            case ets:lookup(line_server, ?LINE_ITEM(Type, random:uniform(N))) of
                [] ->
                    undefined;
                [{_, Line}] ->
                    Line
            end
    end.


-record(state, {}).

%% @private
init(Args) ->
    _Tid = ets:new(line_server, [named_table]),

    load_files(Args),

    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.

%% @private
load_files([]) ->
    ok;
load_files([{Type, Path}|Rest]) ->
    Proc = fun(Line0, I) ->
        Line1 = string:strip(Line0, right, $\n),
        ets:insert(line_server, {?LINE_COUNT(Type), I}),
        ets:insert(line_server, {?LINE_ITEM(Type, I), Line1}),
        I + 1
    end,
    Filename = filename:join(base_path(), Path),
    for_each_line_in_file(Filename, Proc, [read], 1),

    load_files(Rest).

%% @private
for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

%% @private
for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
            for_each_line(Device, Proc, NewAccum)
    end.

base_path() ->
    ?LINE_SERVER_BASE_PATH.
