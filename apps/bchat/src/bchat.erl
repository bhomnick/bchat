-module(bchat).
-export([get_client/1, get_room/1, join_room/2, leave_room/2, send_msg/3]).

%%% PUBLIC API

%% Starts a new client process registered as atom `Name`.
get_client(Name) when is_atom(Name) ->
    start_proc({bchat_client_sup, Name}).

%% Starts a new room process registered as atom `Name`.
get_room(Name) when is_atom(Name) ->
    start_proc({bchat_room_sup, Name}).

%% `Client` joins chat room `Room`.  In theory will return chat history.
join_room(Client, Room) ->
    gen_server:call(Room, {join_room, Client}).

%% `Client` leaves chat room `Room`.
leave_room(Client, Room) ->
    gen_server:call(Room, {leave_room, Client}).

%% `Client` sends `Msg` to all members of `Room`.
send_msg(Client, Room, Msg) ->
    case gen_server:call(Room, {send_msg, Client, Msg}) of
        not_member ->
            io:format("Can't send msg: ~p is not a member of ~p.~n", [Client, Room]);
        R ->
            R
    end.

%%% PRIVATE
start_proc({Sup, Name}) ->
    case whereis(Name) of
        undefined ->
            supervisor:start_child(Sup, [Name]),
            Name;
        _ ->
            io:format("Sorry this name is not available.~n"),
            ok
    end.
