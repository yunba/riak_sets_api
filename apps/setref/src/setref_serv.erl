%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(setref_serv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([item_in_set/3]).
-export([add_to_set/3]).

%-export([create_set/2, add_to_set/2, item_in_set/2, delete_item_from_set/2]).
%-export([create_set/2, add_to_set/2, item_in_set/2, delete_item_from_set/2]).
-define(SERVER, ?MODULE).



%%%===================================================================
%%% API
%%%===================================================================
item_in_set(Pid,Set, Item) ->
    case gen_server:call(Pid, {item_in_set, Set, Item}) of
        true -> true;
        [] -> 
            false;
        fail ->
            false
                
    end.

add_to_set(Pid, Set, Item) ->
    [] =  gen_server:call(Pid, {add_to_set, Set, Item}),
    true.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link( ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, File}   = get_file(),
    {ok, Erlog}  = erlog:new(),
    {ok, Erlog1} = erlog:consult(File, Erlog),
    {ok,  Erlog1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call( PLTERM, _From, Erlog) ->
    case erlog:prove(PLTERM, Erlog) of
        {{succeed, Reply}, Erlog1} ->
            
            {reply, Reply, Erlog1};
        {fail,Erlog1} ->
            {reply, fail, Erlog1}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(reconsult, Erlog) ->
    {ok, File}   = get_file(),
    {ok, Erlog1} = erlog:reconsult(File, Erlog),
    {noreply, Erlog1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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


get_file() ->
    File = code:lib_dir(setref) ++ "/prolog" ++ "/sets.pl",
    {ok, File}.
