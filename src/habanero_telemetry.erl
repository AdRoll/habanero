-module(habanero_telemetry).
-author('James Kelly <jim@adroll.com>').

-include("habanero.hrl").

%% API.
-export([telemetry/0]).
-export([telemetry/1]).

-export([save_to_disk/1]).
-export([base_path/0]).

%% API.

%% @doc Returns all telemetry data as a JSON-able struct.
telemetry() ->
    telemetry(os:timestamp()).

telemetry(Now) ->
    {struct, [
        {<<"timestamp">>, ?SECONDS(Now)},
        {<<"stages">>, stages()},
        {<<"snapshot">>, metrics()}
    ]}.

%% @doc Takes a snapshot of the current telemetry state and saves it to disk.
save_to_disk(Name) when is_integer(Name) ->
    save_to_disk(erlang:integer_to_list(Name));
save_to_disk(Name) ->
    FilePath = filename:join(base_path(Name), get_filename()),
    case file:write_file(FilePath, mochijson2:encode(telemetry())) of
        ok ->
            lager:info("Took snapshot ~p", [FilePath]);
        {error, Reason} ->
            lager:error("Snapshot error ~p", [Reason])
    end.

%% Internal.

%% @doc Returns all tracked metrics.
metrics() ->
    metrics(folsom_metrics:get_metrics_info(), []).

%% @doc Returns the names of all pipeline stages.
stages() ->
    stage_ids(proplists:get_value(pipeline, ?MODULE_ENV(habanero_coordinator)), []).

%% @private
metrics([], A) ->
    A;
metrics([{Name, [{type, counter}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_metric_value(Name)}|A]);
metrics([{Name, [{type, gauge}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_metric_value(Name)}|A]);
metrics([{Name, [{type, histogram}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, filter(folsom_metrics:get_histogram_statistics(Name), [])}|A]);
metrics([{Name, [{type, history}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_history_values(Name, 1024)}|A]);
metrics([{Name, [{type, meter}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_metric_value(Name)}|A]);
metrics([{Name, [{type, meter_reader}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_metric_value(Name)}|A]);
metrics([{Name, [{type, duration}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_metric_value(Name)}|A]);
metrics([{Name, [{type, spiral}, {tags, _}]}|Rest], A) ->
    metrics(Rest, [{Name, folsom_metrics:get_metric_value(Name)}|A]).

%% @doc Filters histogram statistics to convert 'percentile and 'histogram'
%% metrics into JSON-able formats.
filter([], A) ->
    A;
filter([{percentile, Percentiles}|Rest], A) ->
    filter(Rest, [{percentile, [{to_list(Percentile), Value} || {Percentile, Value} <- Percentiles]}|A]);
filter([{histogram, Percentiles}|Rest], A) ->
    filter(Rest, [{histogram, [{to_list(Percentile), Value} || {Percentile, Value} <- Percentiles]}|A]);
filter([Statistic|Rest], A) ->
    filter(Rest, [Statistic|A]).


%% @doc Because R15B03 doesn't support float_to_list/2 apparently.
to_list(F) ->
    [L] = io_lib:format("~p", [F]),
    L.

%% @private
get_filename() ->
    get_filename(erlang:now()).

get_filename(Now) ->
    io_lib:format("~p.json", [?SECONDS(Now)]).

%% @private
make_dirs(Path) ->
    ok = make_dirs(filename:split(Path), []).

%% @private
make_dirs([], _) ->
    ok;
make_dirs([Dir|Rest], A) ->
    A2 = A ++ [Dir],
    case file:make_dir(filename:join(A2)) of
        ok ->
            make_dirs(Rest, A2);
        {error, eexist} ->
            make_dirs(Rest, A2);
        {error, Reason} ->
            {error, Reason}
    end.

base_path() ->
    ?TELEMETRY_BASE_PATH.

base_path(Name) ->
    Path = filename:join(base_path(), Name),
    make_dirs(Path),
    Path.

stage_ids([], A) ->
    lists:reverse(A);
stage_ids([{StageId, _}|Rest], A) ->
    stage_ids(Rest, [StageId|A]).
