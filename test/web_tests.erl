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
-define(PORT, "8080").
-define(HOST, "http://127.0.0.1").

value()		-> quickcheck_util:set_guid().
set_value()	-> value().
set_key()	-> value().


get(Host, Key) ->
    URL = Host ++ "/set/" ++ Key,
    {_, StatusCode, RespHeaders, Ref} = hackney:get( URL),
    {ok, Body} = hackney:body(Ref),

    {ok, StatusCode,RespHeaders,Body}.


get(Host, Key, Value) ->
    URL = Host ++ "/set/" ++ Key ++ "/" ++ Value,
    {_, StatusCode, RespHeaders, Ref} = hackney:get( URL),
    {ok, Body} = hackney:body(Ref),

    {ok, StatusCode,RespHeaders,Body}.



post(Host, Key, Value) ->
    URL = Host ++ "/set/" ++ Key ++ "/" ++ Value,
    {_, StatusCode, RespHeaders, Ref} = hackney:post( URL,[{<<"Content-Type">>, <<"application/json">>}],  <<" ">>),
 
    {ok, Body} = hackney:body(Ref),

    {ok, StatusCode,RespHeaders,Body}.


delete(_Host, _Key, _Value) ->
    true.

delete(_Host, _Key) ->
    true.

%% May want to make it rotate around a cluster later
web_host() ->
    ?HOST ++ ":" ++ ?PORT.

command(_) -> 
    oneof([
	   {call, ?MODULE, get,    [web_host(), set_key(), set_value()]},
	  % {call, ?MODULE, get,    [web_host(), set_key()]},
	   {call, ?MODULE, post,   [web_host(), set_key(), set_value()]}
%	   {call, ?MODULE, delete, [web_host(), set_key(), set_value()]},
%	   {call, ?MODULE, delete, [web_host(), set_key()]}
	  ]).

precondition(_,_)	->    true.
initial_state()		->    sets:new().


next_state(S,_V, {call, _, post, [_, Key, Value]}) ->
    
    sets:add_element({Key, Value}, S);
next_state(S,_V, {call, _, delete, [_, Key, Value]}) ->
    sets:del_element({Key, Value}, S);

next_state(S,_V, _Cmd)	->    S.
 

postcondition(_S, {call,_,_, _},{error, HTTPStatus, _ , _Body}) ->
    ?debugFmt("Error Status ~p Body ~s~n", [HTTPStatus, _Body]),
    false;

postcondition(_S, {call,_,get, [_,_Key]},    _HTTPResult = {ok, _HTTPStatus, _ , Body}) ->
    %?debugFmt("Body ~p~n",[Body]),
    true;

postcondition(S, { call,_,get, [_,Key, Value]},HTTPResult = {ok, HTTPStatus, _ , _Body}) ->    
    case {sets:is_element({Key,Value},S),HTTPStatus} of
	{false, 404} ->
	    true;
	{true, 200}  ->
	    true;
	{false, _Status} = _S -> 
	    ?debugFmt("GET Result = ~p", [HTTPResult]),
	    ?debugFmt("Bad Output ~p", [_S]),
	    false
    end;
postcondition(_S, {call, _, post, [_, _Key, _Value]}, {ok, Status, _,_}) ->

    lists:member(Status, [200, 201,202,204]);
postcondition(_S,_Cmd,_Result) ->
    %?debugFmt("Command ~p", [_Cmd]),
    %?debugFmt("Result ~p", [_Result]),
    true.


prop_run_web() ->
    ?FORALL(Cmds,
	    non_empty(commands(?MODULE)),
	    begin
                gen_server:cast(setref_serv,'reset'),
		{_Start,_End,Result} = run_commands(?MODULE,Cmds),
		?WHENFAIL(begin
			      ?debugFmt("~nResult ~p", [Result]),
			      quickcheck_util:print_cmds(Cmds,0),
			      false
			  end,
			  Result == ok)
			  
	    end).

run_test_() ->
    application:ensure_all_started(restc),
    hackney:start(),
    {timeout, 3600,
     ?_assertEqual([],proper:module(?MODULE,[100,{to_file, user}]))}.
