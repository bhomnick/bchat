-module(bchat_http).
-export([init/3, handle/2, info/3, terminate/2]).

-export([get_client/2, get_room/2, join_room/2, send_msg/2]).

-include("bchat.hrl").

-define(TIMEOUT, 30000).
-define(CB, cowboy_http_req).

init({tcp, http}, Req, _Opts) ->
    {[Path|_], Req2} = ?CB:path(Req),
    {Params, Req3} = ?CB:body_qs(Req2),
    
    %% Do we want to pull the client Pid out here or leave it up to the handlers
    %% to deal with?  Some requests can be anonymous, but most are going to 
    %% want a running client process...
    CPid = case proplists:get_value(<<"cid">>, Params) of
        undefined ->
            undefined;
        Uuid ->
            gproc:where(?GCL(Uuid))
    end,

    %% loop on poll requests, otherwise pass on to the handlers
    case Path of 
        <<"poll">> ->
            handle_poll(Req3, {CPid, Params, Path});
        _ ->
            {ok, Req3, {CPid, Params, Path}}
    end.

%% handle everything but poll requests
handle(Req, S={CPid, Params, Path}) ->
    Resp = erlang:apply(?MODULE, binary_to_atom(Path, utf8), [CPid, Params]),
    {ok, Req2} = ?CB:reply(200, [], jsx:to_json(Resp), Req),
    {ok, Req2, S}.

%% handle poll requests
handle_poll(Req, {CPid, Params, Path}) when CPid =/= undefined ->
    ok = gen_server:call(CPid, {listen, self()}),
    {loop, Req, {CPid, Params, Path}, ?TIMEOUT, hibernate};
handle_poll(Req, S={undefined, _, _}) ->
    {ok, Req2} = cowboy_http_req:reply(400, [], "Invalid client ID.", Req),
    {shutdown, Req2, S}.

%% poll responses
info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], term_to_binary(Body), Req),
    {ok, Req2, State};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Req, {undefined, _, _}) ->
    ok;
terminate(_Req, {CPid, _, _}) ->
    gen_server:call(CPid, {unlisten, self()}),
    ok.

%% the actual handlers!
%% these get the client Pid (could be 'undefined'!) and a proplist of post params, 
%% response should be jsx:to_json-able

get_client(_, _) ->
    {ok, Uuid} = bchat:get_client(),
    [{cid, Uuid}].

get_room(_, Params) ->
    {ok, Uuid} = bchat:get_room(proplists:get_value(<<"rid">>, Params)),
    [{rid, Uuid}].
    
join_room(CPid, Params) ->
    RoomUuid = proplists:get_value(<<"rid">>, Params),
    Nickname = proplists:get_value(<<"nickname">>, Params),
    RPid = gproc:where(?GRM(RoomUuid)),
    ok = bchat:join_room(CPid, RPid, Nickname),
    [{rid, RoomUuid}, {nickname, Nickname}].

send_msg(CPid, Params) ->
    RoomUuid = proplists:get_value(<<"rid">>, Params),
    RPid = gproc:where(?GRM(RoomUuid)),
    Msg = proplists:get_value(<<"msg">>, Params),
    ok = bchat:send_msg(CPid, RPid, Msg),
    <<"ok">>.
