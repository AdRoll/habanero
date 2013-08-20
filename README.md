# habañero

Habanero is a lightweight, HTTP load testing tool written in Erlang.

## Quick start

Habanero requires Erlang R15B03 or higher. On OS X, Erlang is available via [Homebrew](http://brew.sh/).

```shell
brew install erlang
```

Once Erlang is installed, run make to compile habanero:

```shell
make
```

Habanero can be started in interactive mode:

```shell
./habanero.sh 
```

...or detached:

```shell
./habanero.sh -detached
```

By default, the habanero dashboard is available at http://0.0.0.0:5566.


## Configuring Tests

### Pipelines

The core of habanero's execution model is the pipeline. This configurable in <em>habanero.config</em>.

```erlang
%% Sample pipeline executes a GET request on index.html on localhost.
{pipeline, [
    {sample_get, [
        %% Returns [Method, Url, Headers]
            {compose_request, {
                return, [
                    get,
                    "http://localhost/index.html",
                [{"User-Agent", "Habanero/1.0"}]
            ]}
            },
        {transition, {return, sample_get}}
    ]}
]}

```

#### Stages

Pipelines are composed of one or more stages. You can imagine a pipeline as a directed graph where each node represents an HTTP request. The above example shows a simple pipeline with a single request.

Each stage is comprised of two configurable phases: <em>compose request</em> and <em>transition</em>.

#### Phases

Compose request defines the request parameters. This phase must return a list of one of two forms.

An empty body request, e.g. GET or HEAD, in the form [Method, Url, Headers]:

```erlang
{compose_request, {
    return, [get,"http://localhost/index.html",[{"User-Agent", "Habanero/1.0"}]]}
},
```

Or a request with a provided body, in form [Method, Url, Headers, ContentType, Body]:

```erlang
{compose_request, {
    return, [post,"http://localhost/newpost",[{"User-Agent", "Habanero/1.0"}], "text/plain", "foo=bar"]}
},
```

Transition must return a single value specifying the next stage to execute:

```erlang
{transition, {return, sample_get}}
```

### Running tests

Test runs can be pre-configured as a series of worker counts over time.

```erlang
{timeline, [
    {5, 10},
    {15, 10},
    {20, 20},
    {30, 20}
]},
```

The above example defines a 30 second test run. Points in the timeline are specified as tuples of {second, worker count}. Habanero will interpolate the worker values between points spawning or terminating workers as necessary.

You can execute a pre-configured test run via ```start_run()```

```erlang
1> habanero_coordinator:start_run().
ok
2>
```

Test runs can be stopped prematurely via ```stop_run()```

```erlang
1> habanero_coordinator:stop_run().
ok
2>
```

Ad hoc test runs can also be excecuted from via the shell as follows, the arguement to ```workers()``` is the number of workers to spawn:

```erlang
1> habanero_coordinator:workers(1).
ok
2> 17:41:00.507 [info] Spawned worker <0.119.0>
```

## Web Endpoints

Habanero exposes the following web API endpoints:

<dl>
  <dt>/api/start</dt>
  <dd>Start a test run.</dd>
  
  <dt>/api/stop</dt>
  <dd>Stop a test run.</dd>
  
  <dt>/api/history</dt>
  <dd>Fetch a JSON summary of past test runs.</dd>
  
  <dt>/api/poll</dt>
  <dd>Fetch current telemetry data.</dd>

  <dt>/api/load?id=RUN_ID</dt>
  <dd>Fetch telemetry data for a past test run, RUN_ID is the timestamp of the prior run.</dd>
</dl>
