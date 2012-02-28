-module(bchat_room).
-behaviour(gen_server).

-include("bchat.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).

-define(HISTORY_LEN, 5).
-record(state, {
    uuid, 
    clients=dict:new(), 
    history=queue:new()
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Uuid) ->
    gproc:reg(?GRM(Uuid)),
    {ok, #state{uuid=Uuid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%% {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({test, Message}, _From, State) ->
    io:format("Call: ~p~n", [Message]),
    {reply, ok, State};
handle_call({join_room, Client, Nickname}, _From, S=#state{clients=C, history=H, uuid=U}) ->
    send_history(Client, H, U),
    NewS = S#state{clients=dict:store(Client, Nickname, C)},
    {reply, ok, NewS};
handle_call({send_msg, Client, Msg}, _From, S=#state{history=H, clients=C, uuid=U}) ->
    case dict:is_key(Client, C) of
        true ->
            Nickname = dict:fetch(Client, C),
            NewH = enqueue({Nickname, Msg}, H, ?HISTORY_LEN),
            send_msgs(C, Nickname, U, Msg),
            {reply, ok, S#state{history=NewH}};
        false ->
            {reply, not_member, S}
    end;
handle_call(Request, _From, State) ->
    ?INFO({unexpected_call, Request}),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?INFO({unexpected_cast, Msg}),
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?INFO({unexpected_info, Info}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Send chat history to client
send_history(To, H, Room) ->
    case queue:out_r(H) of
        {empty, _} ->
            ok;
        {{value, {From, Msg}}, NewQ} ->
            send_msg(To, From, Room, Msg),
            send_history(To, NewQ, Room)
    end.

%% Send the same message to multiple clients
send_msgs(Clients, From, Room, Msg) ->
    dict:fold(fun(To, _, _) -> send_msg(To, From, Room, Msg), ok end, ok, Clients).

%% Actually send a message
send_msg(To, From, Room, Msg) ->
    gen_server:cast(To, {msg, From, Room, Msg}).

%% Enqueues `Item` to `Queue` while keeping the length of the resulting Queue
%% at most `Max`
enqueue(Item, Queue, Max) ->
    Pushed = queue:in(Item, Queue),
    Fun = fun(_, Acc) -> {_, Popped} = queue:out(Acc), Popped end,
    case queue:len(Pushed) of
        Len when Len > Max ->
            lists:foldl(Fun, Pushed, lists:seq(1, Len-Max));
        _ -> 
            Pushed
    end.
    
