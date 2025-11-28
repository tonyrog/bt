-ifndef(__BT_LOG_HRL__).
-define(__BT_LOG_HRL__, true).

-define(LOG_LEVEL_NONE, -1).
-define(LOG_LEVEL_EMERGENCY, 0).
-define(LOG_LEVEL_ALERT,     1).
-define(LOG_LEVEL_CRITICAL,  2).
-define(LOG_LEVEL_ERROR,     3).
-define(LOG_LEVEL_WARNING,   4).
-define(LOG_LEVEL_NOTICE,    5).
-define(LOG_LEVEL_INFO,      6).
-define(LOG_LEVEL_DEBUG,     7).

-define(BT_LOG_LEVEL, ?LOG_LEVEL_DEBUG).

-define(debug(Fmt),   ?bt_log(?LOG_LEVEL_DEBUG,"DEBUG: " Fmt ,[])).
-define(warning(Fmt), ?bt_log(?LOG_LEVEL_WARNING,"WARNING: " Fmt,[])).
-define(info(Fmt),    ?bt_log(?LOG_LEVEL_INFO,"INFO: " Fmt,[])).
-define(error(Fmt),   ?bt_log(?LOG_LEVEL_ERROR,"ERROR: "Fmt,[])).

-define(debug(Fmt, As),   ?bt_log(?LOG_LEVEL_DEBUG,"DEBUG: " Fmt ,As)).
-define(warning(Fmt, As), ?bt_log(?LOG_LEVEL_WARNING,"WARNING: " Fmt,As)).
-define(info(Fmt, As),    ?bt_log(?LOG_LEVEL_INFO,"INFO: " Fmt,As)).
-define(error(Fmt, As),   ?bt_log(?LOG_LEVEL_ERROR,"ERROR: "Fmt,As)).

-define(bt_log(Level, Fmt, As),
	case Level =< ?BT_LOG_LEVEL of
	    true ->
		io:format(Fmt "\n", As);
	    false ->
		ok
	end).


-endif.
