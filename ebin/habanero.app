{application, habanero,
 [
  {description, "A light weight loadtesting framework."},
  {vsn, "1.0.0"},
  {registered, []},
  {applications, [kernel, stdlib, inets, ssl, folsom, cowboy, lager]},
  {mod, {habanero_app, []}},
  {env, []},
  {modules, [habanero, habanero_app, habanero_coordinator,
            habanero_deps, habanero_handler, habanero_http_worker,
            habanero_line_server, habanero_sup,
            habanero_telemetry]}
 ]
}.
