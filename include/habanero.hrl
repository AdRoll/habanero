-author('James Kelly <jim@adroll.com>').

-define(TELEMETRY_BASE_PATH, "priv/history").
-define(LINE_SERVER_BASE_PATH, "priv/data").

-define(MODULE_ENV(Module), proplists:get_value(Module, application:get_all_env(habanero), [])).

-define(SECONDS(Time),
        case Time of
            {MegaSecs, Secs, _} -> trunc((1.0e+6 * MegaSecs) + Secs)
        end).
