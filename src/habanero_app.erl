-module(habanero_app).
-author('James Kelly <jim@adroll.com>').

-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% API.
start(_StartType, _StartArgs) ->
    % Set httpc options
    case application:get_env(habanero, http_options) of
        {ok, HttpOptions} ->
            httpc:set_options(HttpOptions);
        _ ->
            lager:warn("http_options undefined, using defaults!")
    end,

    % Compile cowboy routes
    Routes = cowboy_router:compile([{'_', [
        {"/api/:action", habanero_handler, []},
        {"/js/[...]", cowboy_static, [
            {directory, {priv_dir, habanero, [<<"static/js">>]}},
            {mimetypes, [{<<".js">>, [<<"text/javascript">>]}]}
        ]},
        {"/css/[...]", cowboy_static, [
            {directory, {priv_dir, habanero, [<<"static/css">>]}},
            {mimetypes, [{<<".css">>, [<<"text/css">>]}]}
        ]},
        {"/img/[...]", cowboy_static, [
            {directory, {priv_dir, habanero, [<<"static/img">>]}},
            {mimetypes, [{<<".png">>, [<<"image/png">>]}]}
        ]},
        {"/", cowboy_static, [
            {directory, {priv_dir, habanero, [<<"static">>]}},
            {file, "index.html"},
            {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
        ]}
    ]}]),

    % Start cowboy
    {ok, _Pid} = cowboy:start_http(habanero_listener, 1, [{port, 5566}],
        [{env, [{dispatch, Routes}]}]),

    habanero_sup:start_link().

stop(_State) ->
    ok.