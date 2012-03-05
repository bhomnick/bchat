-module(bchat_http).
-export([init/3, handle/2, info/3, terminate/2]).

-export([get_client/1, get_room/1, join_room/1, send_msg/1]).

-include("bchat.hrl").

-define(TIMEOUT, 30000).
-define(CB, cowboy_http_req).

init({tcp, http}, Req, _Opts) ->
    {[Path|_], Req2} = ?CB:path(Req),
    {Params, Req3} = ?CB:body_qs(Req2),
    CPid = gproc:where(?GCL(proplists:get_value(<<"cid">>, Params))),

    %% set headers
    {ok, Req4} = ?CB:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req3),
    {ok, Req5} = ?CB:set_resp_header(<<"Access-Control-Allow-Headers">>, 
        <<"Origin, X-Requested-With, Accept">>, Req4),

    %% loop on poll requests, otherwise pass on to the handlers
    case Path of 
        <<"poll">> ->
            handle_poll(Req5, {CPid, Params, Path});
        _ ->
            {ok, Req5, {CPid, Params, Path}}
    end.

%% handle everything but poll requests
handle(Req, S={_, Params, Path}) ->
    Resp = erlang:apply(?MODULE, binary_to_atom(Path, utf8), [Params]),
    {ok, Req2} = ?CB:reply(200, [{'Content-Type', <<"application/json">>}], jsx:to_json(Resp), Req),
    {ok, Req2, S}.

%% handle poll requests
handle_poll(Req, S={undefined, _, _}) ->
    {ok, Req2} = cowboy_http_req:reply(400, [], "Invalid client ID.", Req),
    {shutdown, Req2, S};
handle_poll(Req, {CPid, Params, Path}) ->
    ok = gen_server:call(CPid, {listen, self()}),
    {loop, Req, {CPid, Params, Path}, ?TIMEOUT, hibernate}.


%% poll responses
info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], Body, Req),
    {ok, Req2, State};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Req, {undefined, _, _}) ->
    ok;
terminate(_Req, {CPid, _, _}) ->
    gen_server:call(CPid, {unlisten, self()}),
    ok.

%% the actual handlers!
%% these get a proplist of post params, response should be jsx:to_json-able

get_client(_) ->
    {ok, Uuid} = bchat:get_client(),
    [{cid, Uuid}].

get_room(Params) ->
    {ok, Uuid} = bchat:get_room(proplists:get_value(<<"rid">>, Params)),
    [{rid, Uuid}].
    
join_room(Params) ->
    Rid = proplists:get_value(<<"rid">>, Params),
    Nickname = proplists:get_value(<<"nickname">>, Params),
    ok = bchat:join_room(
        proplists:get_value(<<"cid">>, Params),
        Rid,
        Nickname
    ),
    [{rid, Rid}, {nickname, Nickname}].

send_msg(Params) ->
    ok = bchat:send_msg(
        proplists:get_value(<<"cid">>, Params),
        proplists:get_value(<<"rid">>, Params),
        proplists:get_value(<<"msg">>, Params)
    ),
    <<"ok">>.
