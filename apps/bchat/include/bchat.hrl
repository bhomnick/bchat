-define(DBG(Term), lager:debug("~p ~p", [self(), Term])).
-define(ERROR(Term), lager:error("~p ~p", [self(), Term])).
-define(INFO(Term), lager:log(info, self(), "~p ~p", [self(), Term])).
-define(INFOS(Term), lager:log(info, self(), "~p ~s", [self(), Term])).

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
