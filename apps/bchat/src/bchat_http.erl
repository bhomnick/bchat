-module(bchat_http).
-export([init/3, handle/2, info/3, terminate/2]).

-export([get_client/2, get_room/2, join_room/2, send_msg/2]).

-define(TIMEOUT, 30000).
-define(CB, cowboy_http_req).

init({tcp, http}, Req, _Opts) ->
    {[Client|[Path|_]], Req2} = ?CB:path(Req),
    Client2 = binary_to_atom(Client, utf8),
    
    %% loop on poll requests, otherwise pass on to the handlers
    case Path of 
        <<"poll">> ->
            ok = gen_server:call(Client2, {listen, self()}),
            {loop, Req2, {Client2, Path}, ?TIMEOUT, hibernate};
        _ ->
            {ok, Req2, {Client2, Path}}
    end.

%% handle everything but poll requests
handle(Req, S={C, P}) ->
    {Params, Req2} = ?CB:body_qs(Req),
    Resp = erlang:apply(?MODULE, binary_to_atom(P, utf8), [C, Params]),
    {ok, Req3} = ?CB:reply(200, [], jsx:to_json(Resp), Req2),
    {ok, Req3, S}.

%% poll responses only
info({reply, Body}, Req, Client) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], term_to_binary(Body), Req),
    {ok, Req2, Client};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Req, {none, _}) ->
    ok;
terminate(_Req, {Client, _}) ->
    ok = gen_server:call(Client, {unlisten, self()}),
    ok.

%% the actual handlers!
%% these get the requested client (could be none!) and a proplist of post params, 
%% response should be jsx:to_json-able

get_client(_, Params) ->
    Name = binary_to_atom(proplists:get_value(<<"name">>, Params), utf8),
    atom_to_binary(bchat:get_client(Name), utf8).

get_room(_, Params) ->
    Room = binary_to_atom(proplists:get_value(<<"room">>, Params), utf8),
    atom_to_binary(bchat:get_room(Room), utf8).

join_room(C, Params) ->
    Room = binary_to_atom(proplists:get_value(<<"room">>, Params), utf8),
    atom_to_binary(bchat:join_room(C, Room), utf8).

send_msg(C, Params) ->
    Room = binary_to_atom(proplists:get_value(<<"room">>, Params), utf8),
    Msg = proplists:get_value(<<"msg">>, Params),
    atom_to_binary(bchat:send_msg(C, Room, Msg), utf8).