-module(bchat).
-export([get_client/0, get_room/1, join_room/3, send_msg/3]).

%%% PUBLIC API

%% Starts a new client process and returns its UUID
get_client() ->
    Uuid = list_to_binary(bchat_util:format_uuid(bchat_util:random_uuid())),
    {ok, _} = supervisor:start_child(bchat_client_sup, [Uuid]),
    {ok, Uuid}.

%% Gets (starts) room process registered as `Uuid` (binary string)
get_room(Uuid) ->
    case gproc:where({n, l, {room, Uuid}}) of
        undefined ->
            {ok, _} = supervisor:start_child(bchat_room_sup, [Uuid]);
        _ -> 
            ok
    end,
    {ok, Uuid}.

%% `Client` and `Room` in these functions should be gproc keys.  `Msg` should probably
%% be a binary string.

%% `Client` joins chat room `Room`.
join_room(Client, Room, Nickname) ->
    gen_server:call(Room, {join_room, Client, Nickname}).

%% `Client` sends `Msg` to all members of `Room`.
send_msg(Client, Room, Msg) ->
    gen_server:call(Room, {send_msg, Client, Msg}).
