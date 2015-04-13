%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(set_ops_tests).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

uuid() ->
    ?LET(Seed,binary(),
         begin
             uuid:uuid_to_string(uuid:get_v5(Seed))
         end).

set_guid() ->
    oneof(["83a62c10-4f1b-43a5-b22c-d63c1e3abe83",
           "de5ac45d-cc99-4be5-959a-8dcd4c809b47",
           "92c07f58-ab90-4b8e-b966-a1610b59addf",
           "ca480e7a-ef96-4dce-bf5c-396be9851e91",
           "a4a1365f-0bd2-405d-9a5c-a6f4f898f5ec",
           "b4b21542-4a6f-4bbc-af12-9c651f0f0c29",
           "844fe55d-b9e3-4788-9fa9-3f2cd24c9f9c",
           "74293212-b2bb-4542-8d2a-f7a987f3cce5",
           "d5828407-77a4-49fc-bdfa-f8539b0565ac",
           "3332c75d-98b9-4b62-9573-6450a96f83bc"]).

evil_real() ->
    frequency([
               {20, real()},
               {1, return(0.0)},
               {1, return(0.0/-1)}]).

value() -> set_guid().

    %% frequency([{100, oneof([int(),binary(), char(), uuid(), vector(10, char())])},
    %%            {10, evil_real()},
    %%            {50, set_guid()}]).



    
set_value() -> value().
set_key()   -> value().

backends() ->
    oneof([{setref_serv,fun() ->
                                setref_serv:start_link()
                        end}]).

prop_save_and_exists() ->
    ?FORALL({Key, Value},
            {set_key(), set_value()},
            begin
                Backend = setref_serv,
                setref_serv:start_link(),
                Pid   = setref_serv,
                gen_server:cast(setref_serv,'reset'),
                false =  Backend:item_in_set(Pid, Key, Value),
                Backend:add_to_set(Pid, Key, Value),
                true  =  Backend:item_in_set(Pid, Key, Value),
                true

            end).

prop_run_commands() ->
    ?FORALL(Cmds,
            non_empty(commands(?MODULE)),
            ?TRAPEXIT(
               begin
                   gen_server:cast(setref_serv,'reset'),
                   setref_serv:start_link(),

                   Pid = setref_serv,
                   {_Start,End,ok} = run_commands(?MODULE,Cmds),
                   
                   ?WHENFAIL(
                      begin
                          print_cmds(Cmds,0),
                          io:format(user, "Set Values ~nModel: ~p~nServer: ~p~n",[sets:to_list(End),sets:to_list( gen_server:call(Pid, list))])
                      end,
                      End == gen_server:call(Pid, list))
                       
               end)).


print_cmds([],_) ->
    ok;
print_cmds([Cmd|Rest],N) ->
    io:format(user,"~p  Command: ~p~n",[N,Cmd]),
    print_cmds(Rest,N+1).

get_pid([{set,_,{call,_,_,[Pid|_]}}|_]) ->
    Pid.
get_backend([{set,_,{call,Backend,_,_}}|_]) ->
    Backend.

command(_) ->
    Backend = setref_serv,
    oneof([
           {call, Backend, item_in_set,     [setref_serv, set_key(), set_value()]},
           {call, Backend, add_to_set,      [setref_serv, set_key(), set_value()]},
           {call, Backend, remove_from_set, [setref_serv, set_key(), set_value()]}
          ]).
        


precondition(_,_) ->
    true.

initial_state() ->
    sets:new().

next_state(S,_V, {call, _, add_to_set, [_, Key, Value]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, _Cmd = {call, _, remove_from_set, [_, Key, Value]}) ->
    sets:del_element({Key, Value}, S);
next_state(S,_V, _Cmd) ->
    S.

postcondition(S,{call,_,item_in_set, [_,Key, Value]},Result) ->
    Result == sets:is_element({Key,Value},S);
postcondition(_S,_Cmd,_Result) ->
    true.

start_system_test() ->
    code:add_pathz("../apps/setref/ebin"),
    {ok, Pid} = setref_serv:start_link(),
    ?assert(is_process_alive(Pid)),
    true.


run_test() ->
    application:ensure_all_started(lager),
    code:add_pathz("../apps/setref/ebin"),
    ?assertEqual([],proper:module(?MODULE,[100,{to_file, user}])).
