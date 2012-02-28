-module(bchat_app).
-behaviour(application).

-include("bchat.hrl").

-export([start/0]).
-export([start/2, stop/1]).

start() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(lager),
    ok = application:start(cowboy),
    ok = application:start(gproc),
    application:start(bchat).

start(_Type, _StartArgs) ->
    
    {ok, Pid} = bchat_sup:start_link(),
    {ok, HttpConf} = application:get_env(http),
    
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, ?GV(port, HttpConf)}],
        cowboy_http_protocol, [{dispatch, ?GV(dispatch, HttpConf)}]
    ),
    
    {ok, Pid}.

stop(_State) ->
    ok.
