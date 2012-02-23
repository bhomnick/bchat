-module(bchat_app).
-behaviour(application).

-export([start/0]).

-export([start/2, stop/1]).

start() ->
    ok = application:start(lager),
    ok = application:start(gproc),
    application:start(bchat).


start(_Type, _StartArgs) ->
    case bchat_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
