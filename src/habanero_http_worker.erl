-module(habanero_http_worker).
-author('James Kelly <jim@adroll.com>').

%% API.
-export([start_link/1]).
-export([loop/1]).
-export([stop/1]).

-record(timestamps, {
    loop_start :: erlang:timestamp(),
    request_composed :: erlang:timestamp(),
    received_headers :: erlang:timestamp(),
    received_body :: erlang:timestamp(),
    loop_end :: erlang:timestamp()
}).

-type phase() :: 'compose_request' | 'transition'.
-type phase_definition() :: {phase(), term()}.
-type stage_definition() :: {atom, [phase_definition()]}.
-type pipeline() :: [stage_definition()].

-record(state, {
    pipeline :: pipeline(),
    current_stage :: stage_definition(),
    timestamps = #timestamps{} :: #timestamps{},
    context = [] :: [{atom, list()}],

    query_count = 0,
    last_qps_measure
}).

% Lifecycle events
-define(EVT_LOOP_START, loop_start).
-define(EVT_REQUEST_COMPOSED, request_composed).
-define(EVT_RECEIVED_HEADERS, received_headers).
-define(EVT_RECEIVED_BODY, received_body).
-define(EVT_LOOP_END, loop_end).

% Metrics
-define(INTERVAL_SEND_REQUEST, send_request).
-define(INTERVAL_WAIT, wait).
-define(INTERVAL_PROCESS_RESPONSE, process_response).

-define(INTERVALS, [?INTERVAL_SEND_REQUEST, ?INTERVAL_WAIT, ?INTERVAL_PROCESS_RESPONSE]).

%% API.

%% @doc Spawns a new HTTP Worker process which will execute the given pipeline.
start_link([]) ->
    {error, bad_pipeline};
start_link(Pipeline) ->
    State = #state{
        pipeline = Pipeline,
        current_stage = stage_id(hd(Pipeline)),
        last_qps_measure = pytime()
    },

    % we keep a global spiral tracking overall requests per minute
    folsom_metrics:new_spiral(global_qpm),

    % similarly keep a global sliding histogram tracking overall qps
    folsom_metrics:new_histogram(global_qps, slide, 1),

    % and we'll also try measuring qps ourselves
    folsom_metrics:new_gauge(global_brute_force_qps),

    initialize_histograms(State),

    {ok, erlang:spawn_link(?MODULE, loop, [State])}.

%% @doc Stops the given worker process.
-spec stop(pid()) -> true.
stop(Pid) ->
    erlang:exit(Pid, stopped).

%% @doc Main loop for the HTTP Worker.
loop(State) ->
    loop(compose_request, timestamp(?EVT_LOOP_START, State)).

%% @doc
loop(compose_request, State) ->
    Request = compose_request(State),
    case send_request(Request) of
        {ok, RequestId} ->
            loop(wait,
                timestamp(?EVT_REQUEST_COMPOSED,
                    context_store(State, request, Request)));
        {error, Reason} ->
            lager:error("Request error: ~p", [Reason]),
            {error, Reason}
    end;
loop(wait, #state{current_stage = Stage} = State) ->
    receive
        {http, {RequestId, stream_start, Headers}} ->
            loop(wait, timestamp(?EVT_RECEIVED_HEADERS, context_store(State, body, [])));
        {http, {RequestId, stream, Body}} ->
            loop(wait, context_store(State, body, [context_value(State, body)|Body]));
        {http, {RequestId, stream_end, Headers}} ->
            loop(transition, timestamp(?EVT_RECEIVED_BODY, context_store(State, headers, Headers)));
        {http, {RequestId, {Status, Headers, Body}}} ->
            % Received headers and body in one go, typically a redirect.
            State0 = timestamp(?EVT_RECEIVED_HEADERS, context_store(State, headers, Headers)),
            loop(transition, timestamp(?EVT_RECEIVED_BODY, context_store(State0, body, Body)));
        Message ->
            % Mystery message, clear stored headers & body and continue
            State0 = timestamp(?EVT_RECEIVED_HEADERS, context_store(State, headers, [])),
            loop(transition, timestamp(?EVT_RECEIVED_BODY, context_store(State0, body, <<"">>)))
    end;
loop(transition, State = #state{query_count=QC, last_qps_measure=LastQPSMeasure}) ->
    % record the global request count stats
    folsom_metrics:notify({global_qpm, 1}),
    folsom_metrics:notify({global_qps, 1}),

    % Get next stage
    NextStage = to_atom(transition(State)),
    % Update telemetry data
    State1 = notify_async(?INTERVALS, timestamp(?EVT_LOOP_END, State)),
    State2 = State1#state{
        current_stage = NextStage,
        timestamps = #timestamps{}
    },

    % Loop
    Now = pytime(),
    loop(
        case Now - LastQPSMeasure of
            QPSWindow when QPSWindow >= 10 ->
                folsom_metrics:notify({global_brute_force_qps, trunc(QC+1 / QPSWindow)}),
                State2#state{
                    query_count = 0,
                    last_qps_measure = pytime()
                };
            _ ->
                State2#state{
                    query_count = QC+1
                }
        end
    ).

to_atom(Value) when erlang:is_atom(Value) ->
    Value;
to_atom(Value) when erlang:is_list(Value) ->
    erlang:list_to_atom(Value);
to_atom(Value) when erlang:is_binary(Value) ->
    erlang:list_to_atom(erlang:binary_to_list(Value)).

to_list(Value) when erlang:is_list(Value) ->
    Value;
to_list(Value) when erlang:is_binary(Value) ->
    erlang:binary_to_list(Value).


%% @doc Returns the stage ids for all stages in the pipeline.
stage_ids(#state{pipeline = Pipeline}) ->
    stage_ids(Pipeline, []).

%% @doc Extract stage ids from pipeline.
stage_ids([], A) ->
    A;
stage_ids([Stage|Rest], A) ->
    stage_ids(Rest, [stage_id(Stage)|A]).

%% @doc Return the id for the given stage.
stage_id({StageId, _}) ->
    StageId.

%% @doc Return the current stage given the state of the worker.
current_stage(#state{pipeline = Pipeline, current_stage = StageId}) ->
    proplists:get_value(StageId, Pipeline).

%% @doc Sends an HTTP request.
send_request([Method, Url, Headers]) ->
    send_request(to_atom(Method), to_list(Url), Headers, [], [{sync, false}, {stream, self}]);
send_request([Method, Url, Headers, ContentType, Body]) ->
    send_request(to_atom(Method), to_list(Url), Headers, to_list(ContentType), to_list(Body),
        [{autoredirect, true}], [{sync, false}, {stream, self}]).

%% @doc Sends a HTTP request with an empty body.
send_request(Method, Url, Headers, HTTPOptions, Options) ->
    httpc:request(Method, {Url, Headers}, HTTPOptions, Options).

%% @doc Sends a HTTP request with the given Content-Type and Body.
send_request(Method, Url, Headers, ContentType, Body, HTTPOptions, Options) ->
    httpc:request(Method, {Url, Headers, ContentType, Body}, HTTPOptions, Options).

%% @doc TODO
compose_request(State) ->
    call(proplists:get_value(compose_request, current_stage(State)), State).

%% @doc TODO
transition(State) ->
    call(proplists:get_value(transition, current_stage(State)), State).

%% @doc This is where magic happens.
call(undefined, _State) ->
    undefined;
call({return, Value}, _State) ->
    Value;
call({erlang, {Module, Function}}, #state{context = C} = State) ->
    Module:Function(C);
call({javascript, Function}, #state{context = C} = State) ->
    {_, Result} = case js_driver:new() of
                      {ok, JS} ->
                          Javascript = iolist_to_binary([<<"var habaneroFun = ">>|Function]),
                          case js:define(JS, Javascript) of
                              ok ->
                                  js:call(JS, <<"habaneroFun">>, []);
                              _ ->
                                  lager:error("Javascript error"),
                                  {error, undefined}
                          end;
                      _ ->
                          lager:error("Javascript error"),
                          {error, undefined}
                  end,
    Result.


%% @doc Updates the telemetry metrics asynchronously.
notify_async(Intervals, State) ->
    erlang:spawn(
        fun() ->
            notify(Intervals, State)
        end
    ),
    State.

%% @doc Update telemetry data for the given metrics.
notify([], _State) ->
    ok;
notify([Interval|Rest], #state{timestamps = Timestamps, current_stage = StageId} = State) ->
    Value = trunc(get_interval_value(Interval, Timestamps) / 1.0e3),
    folsom_metrics:notify({metric_name({global, Interval}), Value}),
    folsom_metrics:notify({metric_name({StageId, Interval}), Value}),
    notify(Rest, State).

%% @doc Record the timestamp of a given event.
timestamp(Event, #state{timestamps = Timestamps} = State) ->
    State#state{timestamps = update_timestamp(Event, Timestamps)}.

update_timestamp(?EVT_LOOP_START, Timestamp) ->
    Timestamp#timestamps{loop_start = os:timestamp()};
update_timestamp(?EVT_REQUEST_COMPOSED, Timestamp) ->
    Timestamp#timestamps{request_composed = os:timestamp()};
update_timestamp(?EVT_RECEIVED_HEADERS, Timestamp) ->
    Timestamp#timestamps{received_headers = os:timestamp()};
update_timestamp(?EVT_RECEIVED_BODY, Timestamp) ->
    Timestamp#timestamps{received_body = os:timestamp()};
update_timestamp(?EVT_LOOP_END, Timestamp) ->
    Timestamp#timestamps{loop_end = os:timestamp()}.

%% @doc Determine time elapsed for the given operation.
get_interval_value(?INTERVAL_SEND_REQUEST, #timestamps{loop_start = T1, request_composed = T2}) ->
    timer:now_diff(T2, T1);
get_interval_value(?INTERVAL_WAIT, #timestamps{request_composed = T1, received_headers = T2}) ->
    timer:now_diff(T2, T1);
get_interval_value(?INTERVAL_PROCESS_RESPONSE, #timestamps{received_headers = T1, loop_end = T2}) ->
    timer:now_diff(T2, T1).

initialize_histograms(#state{} = State) ->
    Names = [metric_name({Stage, Interval}) || Stage <- [global|stage_ids(State)], Interval <- ?INTERVALS],
    initialize_histograms(Names);
initialize_histograms([]) ->
    ok;
initialize_histograms([Name|Rest]) ->
    folsom_metrics:new_histogram(Name, exdec),
    initialize_histograms(Rest).

metric_name({Stage, Interval}) ->
    iolist_to_binary([erlang:atom_to_list(Stage), "_", erlang:atom_to_list(Interval)]).

context_store(#state{context = C0, current_stage = CurrentStage} = State, Key, Value) ->
    C1 = lists:keystore(Key, 1, proplists:get_value(CurrentStage, C0, []), {Key, Value}),
    State#state{context = lists:keystore(CurrentStage, 1, C0, {CurrentStage, C1})}.

context_value(#state{context = C0, current_stage = CurrentStage} = State, Key) ->
    proplists:get_value(Key, proplists:get_value(CurrentStage, C0, [])).


pytime() ->
    pytime(os:timestamp()).
pytime({MegaSecs, Secs, MicroSecs}) ->
    (1.0e+6 * MegaSecs) + Secs + (1.0e-6 * MicroSecs).
