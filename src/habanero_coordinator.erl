-module(habanero_coordinator).
-author('James Kelly <jim@adroll.com>').

-behaviour(gen_server).

-include("habanero.hrl").

%% API.
-export([start_link/1]).
-export([workers/1]).
-export([start_run/0]).
-export([stop_run/0]).
-export([status/0]).
-export([execute_timeline/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type second() :: pos_integer().
-type worker_count() :: pos_integer().
-type timeline_definition() :: [{second(), worker_count()}].

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc Adjusts the number of active workers during an ad hoc load test.
-spec workers(pos_integer()) -> ok.
workers(NumWorkers) ->
    gen_server:cast(?MODULE, {workers, NumWorkers}).

%% @doc Starts an automated load test.
-spec start_run() -> ok.
start_run() ->
    gen_server:cast(?MODULE, {start_run}).

%% @doc Stops any active load test.
-spec stop_run() -> ok.
stop_run() ->
    gen_server:cast(?MODULE, {stop_run}).

%% @doc Return true an automated load test is running, false otherwise.
-spec status() -> [true|false].
status() ->
    gen_server:call(?MODULE, {status}).

-record(state, {
    workers = [] :: [pid()],
    pipeline = [] :: list(),
    timeline = [] :: timeline_definition(),
    tref = undefined :: undefined | timer:tref()
}).

init(Args) ->
    process_flag(trap_exit, true),
    _ = reset_metrics(),
    {ok, #state{
        pipeline = proplists:get_value(pipeline, Args, []),
        timeline = proplists:get_value(timeline, Args)
    }}.

%% @private
handle_call({status}, _From, State) ->
    Status = case State of
                 #state{tref = undefined, workers = Workers} ->
                     if
                         length(Workers) > 0 -> adhoc;
                         true -> idle
                     end;
                 _ -> running
             end,
    {reply, Status, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({workers, NumWorkers}, State) ->
    {noreply, notify(rescale(State, NumWorkers))};
handle_cast({start_run}, #state{timeline = Timeline} = State) ->
    {ok, TRef} = timer:apply_interval(1000, habanero_coordinator, execute_timeline,
        [explode_timeline(Timeline), os:timestamp()]),
    {noreply, State#state{tref = TRef}};
handle_cast({stop_run}, State) ->
    _ = case State of
            #state{tref = undefined} -> ok;
            #state{tref = TRef} -> timer:cancel(TRef)
        end,
    {noreply, notify(rescale(State#state{tref = undefined}, 0))};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({workers, NumWorkers}, State) ->
    {noreply, notify(rescale(State, NumWorkers))};
handle_info({'EXIT', Pid, Reason}, State) ->
    _ = lager:info("Worker ~p existed for reason ~p", [Pid, Reason]),
    {noreply, notify(remove_worker(State, Pid))};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.

%% @doc Spawns or terminates workers until the desired worker count is reached.
rescale(#state{workers = Workers} = State, Goal) when length(Workers) =:= Goal, Goal =:= 0 ->
    _ = reset_metrics(),
    State;
rescale(#state{workers = Workers} = State, Goal) when length(Workers) =:= Goal ->
    State;
rescale(#state{workers = Workers} = State, Goal) when length(Workers) < Goal ->
    rescale(spawn_worker(State), Goal);
rescale(#state{workers = Workers} = State, Goal) when length(Workers) > Goal ->
    rescale(terminate_worker(State), Goal).

%% @doc Spawns a new worker process.
spawn_worker(#state{workers = Workers, pipeline = Pipeline} = State) ->
    case habanero_http_worker:start_link(Pipeline) of
        {ok, Pid} ->
            _ = lager:info("Spawned worker ~p", [Pid]),
            State#state{workers = [Pid|Workers]};
        {error, Error} ->
            _ = lager:error("Error spawning worker ~p", [Error]),
            State
    end.

%% @doc Terminates a worker process.
terminate_worker(#state{workers = [Pid| Rest]} = State) ->
    habanero_http_worker:stop(Pid),
    State#state{workers = Rest}.

%% @doc Remove the given worker from the list of workers.
remove_worker(#state{workers = Workers} = State, Pid) ->
    State#state{workers = lists:delete(Pid, Workers)}.

%% @doc Updates the <em>worker_count</em>
notify(#state{workers = Workers} = State) ->
    ok = folsom_metrics:notify({worker_count, erlang:length(Workers)}),
    State.

%% @doc Executes a load test timeline.
%%
%% Adjusts the worker count based on the given timeline. Determines current
%% point in the timeline based on the delta between now and the start of
%% the load test.
-spec execute_timeline(list(pos_integer()), erlang:timestamp()) -> ok.
execute_timeline(Timeline, StartTs) ->
    % Take a snapshot of the current telemetry data.
    ok = habanero_telemetry:save_to_disk(?SECONDS(StartTs)),

    N = erlang:min(trunc(timer:now_diff(os:timestamp(), StartTs) / 1.0e6), erlang:length(Timeline)),

    % Stop the run if we overrun the timeline, otherwise update the worker count
    case length(Timeline) of
        N ->
            stop_run();
        _ ->
            workers(lists:nth(N, Timeline))
    end.

%% @doc Explodes a timeline definition into a list of worker counts.
%%
%% Transforms a timeline definition into a list where each element of the list is
%% the number of workers at that second of the load test.
-spec explode_timeline(timeline_definition()) -> list(worker_count()).
explode_timeline(Timeline) ->
    explode_timeline(Timeline, 1, {0, 0}, []).

explode_timeline([], _I, {_T1, _C1}, A) ->
    lists:reverse(A);
explode_timeline([{T2, C2}|Rest], I, {T1, _C1}, A) when I =:= T2 - T1 ->
    explode_timeline(Rest, 1, {T2, C2}, [C2|A]);
explode_timeline([{T2, C2}|_Rest] = List, I, {T1, C1}, A) when I < T2 - T1 ->
    Incr = (C2 - C1) / (T2 - T1),
    explode_timeline(List, I + 1, {T1, C1}, [trunc(C1 + Incr * I)|A]).

reset_metrics() ->
    delete_metrics(folsom_metrics:get_metrics_info()),
    folsom_metrics:new_gauge(worker_count).

delete_metrics([]) ->
    ok;
delete_metrics([{Name, _}|Rest]) ->
    folsom_metrics:delete_metric(Name),
    delete_metrics(Rest).
