-module(bchat_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RoomSuper = {bchat_room_sup, {bchat_room_sup, start_link, []},
        permanent, 2000, supervisor, [bchat_room_sup]},
    ClientSuper = {bchat_client_sup, {bchat_client_sup, start_link, []},
        permanent, 2000, supervisor, [bchat_client_sup]},
    Children = [RoomSuper, ClientSuper],
    RestartStrategy = {one_for_one, 3, 60},
    {ok, {RestartStrategy, Children}}.
