-module(habanero_handler).
-author('James Kelly <jim@adroll.com>').

-behaviour(cowboy_http_handler).

-include("habanero.hrl").

%% API.
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% API.

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Response} = dispatch(cowboy_req:binding(action, Req)),
    {ok, Response, State}.

terminate(_Reason, _Req, _State) ->
    ok.


%% Internal.

dispatch({<<"poll">>, R}) ->
    json(R, mochijson2:encode(habanero_telemetry:telemetry()));
dispatch({<<"start">>, R}) ->
    habanero_coordinator:start_run(),
    ok(R);
dispatch({<<"stop">>, R}) ->
    habanero_coordinator:stop_run(),
    ok(R);
dispatch({<<"history">>, R}) ->
    {ok, JSON} = history(),
    json(R, JSON);
dispatch({<<"load">>, R}) ->
    {ok, JSON} = load(cowboy_req:qs_val(<<"id">>, R)),
    json(R, JSON);
dispatch({<<"status">>, R}) ->
    echo(R, habanero_coordinator:status());
dispatch({_, R}) ->
    ok(R).

ok(R) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", R).

echo(R, [true]) ->
    echo(R, <<"true">>);
echo(R, [false]) ->
    echo(R, <<"false">>);
echo(R, Msg) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Msg, R).

json(R, Json) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Json, R).

%% @doc Return history metadata for all past runs.
history() ->
    Path = habanero_telemetry:base_path(),
    FileNames = case file:list_dir(Path) of
                    {ok, Fs} -> Fs;
                    _ -> []
                end,
    History = history(Path, sorted(cleaned(FileNames)), []),
    {ok, mochijson2:encode({struct, [
                                     {<<"history">>, History}
                                    ]
                           })
    }.

%% @doc Return history metadata for the given Path and Filenames.
history(_Path, [], A) ->
    A;
history(Path, [Filename|Rest], A) ->
    case file:list_dir(filename:join(Path, Filename)) of
        {ok, FileNames} ->
            JSON = {struct, [
                {<<"date">>, iolist_to_binary(Filename)},
                {<<"duration">>, erlang:length(FileNames)},
                {<<"source">>, <<"">>},
                {<<"target">>, <<"">>}]},
            history(Path, Rest, [JSON|A]);
        {error, Reason} ->
            ok = lager:error("History error ~p, skipping ~p", [Reason, Filename]),
            history(Path, Rest, A)
    end.

cleaned(FileNames) ->
    cleaned(FileNames, []).

cleaned([], A) ->
    A;
cleaned([FileName|Rest], A) ->
    case hd(FileName) of
        $. ->
            cleaned(Rest, A);
        _ ->
            cleaned(Rest, [FileName|A])
    end.

sorted(FileNames) ->
    lists:sort(
        fun(A, B) ->
            erlang:list_to_integer(A) =< erlang:list_to_integer(B)
        end, FileNames).

%% @doc Load telemetry data for a past run.
load({Id, _}) ->
    Path = filename:join(habanero_telemetry:base_path(), Id),
    case file:list_dir(Path) of
        {ok, Files} ->
            {ok, mochijson2:encode(load(Path, Files, []))};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Load all telemetry snapshots for a past run.
load(_Path, [], A) ->
    A;
load(Path, [Filename|Rest], A) ->
    File = filename:join(Path, Filename),
    case file:read_file(File) of
        {ok, Contents} ->
            load(Path, Rest, [mochijson2:decode(Contents)|A]);
        {error, Reason} ->
            ok = lager:error("Load error ~p, skipping ~p", [Reason, File]),
            load(Path, Rest, A)
    end.
