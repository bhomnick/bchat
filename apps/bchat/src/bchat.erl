-module(bchat).
-export([get_client/1, get_room/1, join_room/2, leave_room/2, send_msg/3]).

%%% PUBLIC API

%% Starts a new client process and returns its UUID
get_client(Name) ->
    Uuid = bchat_util:random_uuid(),
    {ok, _} = supervisor:start_child(bchat_client_sup, [{Name, Uuid}]),
    {ok, Uuid}.

%% Gets (starts) room process registered as `Uuid`
get_room(Uuid) ->
    case gproc:where({n, l, {room, Uuid}}) of
        undefined ->
            {ok, _} = supervisor:start_child(bchat_room_sup, [Uuid]);
        _ -> 
            ok
    end,
    {ok, Uuid}.

%% `Client` joins chat room `Room`.  In theory will return chat history.
join_room(Client, Room) ->
    gen_server:call(Room, {join_room, Client}).

%% `Client` leaves chat room `Room`.
leave_room(Client, Room) ->
    gen_server:call(Room, {leave_room, Client}).

%% `Client` sends `Msg` to all members of `Room`.
send_msg(Client, Room, Msg) ->
    gen_server:call(Room, {send_msg, Client, Msg}).
