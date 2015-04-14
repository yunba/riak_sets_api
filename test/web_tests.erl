%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(web_tests).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-define(PORT, 8080).
-define(HOST, "http://127.0.0.1").


command(_) -> 
    oneof([]).
precondition(_,_) ->
    true.

initial_state() ->
    sets:new().

next_state(S,_V, _Cmd) ->
    S.

postcondition(S,{call,_,item_in_set, [_,Key, Value]},Result) ->
    Result == sets:is_element({Key,Value},S);
postcondition(_S,_Cmd,_Result) ->
    true.


prop_run_web() ->
    ?FORALL(Cmds,
	    non_empty(commands(?MODULE)),
	    begin
		{_Start,_End,Result} = run_commands(?MODULE,Cmds),
		?WHENFAIL(begin
			      quickcheck_util:print_cmds(Cmds,0),
			      false
			  end,
			  Result == ok)
			  
	    end).

run_test_() ->

    {timeout, 3600,
     ?_assertEqual([],proper:module(?MODULE,[100,{to_file, user}]))}.
