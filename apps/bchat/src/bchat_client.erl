-module(bchat_client).
-behaviour(gen_server).

-include("bchat.hrl").

%% API
-export([
    start_link/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).

-record(state, {
    uuid,
    cache=[],
    listener=none
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
    gproc:reg(?GCL(Uuid)),
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
handle_call({listen, Pid}, _From, S=#state{cache=C}) when C =/= [] ->
    NewS = flush_cache(S#state{listener=Pid}),
    {reply, ok, NewS};
handle_call({listen, Pid}, _From, S) ->
    {reply, ok, S#state{listener=Pid}};
handle_call({unlisten, Pid}, _From, S=#state{listener=Pid}) ->
    {reply, ok, S#state{listener=none}};
handle_call({unlisten, _}, _From, S) ->
    {reply, ok, S}; %% noop
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
handle_cast({msg, From, Room, Msg}, S=#state{cache=C}) ->
    NewS = flush_cache(S#state{cache=[{From, Room, Msg}|C]}),
    {noreply, NewS};
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

flush_cache(S=#state{listener=none}) ->
    S;
flush_cache(S=#state{listener=L, cache=C}) ->
    L ! {reply, jsx:to_json([format_msg(M) || M <- C])},
    S#state{listener=none, cache=[]}.

%% Makes jsx happy!
%% atom, atom, binary
format_msg({From, Room, Msg}) ->
    [
        {from, From},
        {room, Room},
        {msg, Msg}
    ].