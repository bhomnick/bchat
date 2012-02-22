-module(bchat_room_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
        [{bchat_room,
            {bchat_room, start_link, []},
            temporary, 1000, worker, [bchat_room]}
        ]}}.
