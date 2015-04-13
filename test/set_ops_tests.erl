%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(set_ops_tests).
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

value() ->
    frequency([{100, oneof([int(),binary(), char(), uuid(), vector(10, char())])},
               {10, evil_real()},
               {10, set_guid()}]).



    
set_value() -> value().
set_key()   -> value().

backends() ->
    oneof([{setref_serv,fun() ->
                                setref_serv:start_link()
                        end}]).

prop_save_and_exists() ->
    ?FORALL({Key, Value, {Backend, StartFunc}},
            {set_key(), set_value(), backends()},
            begin
                {ok, Pid}   = StartFunc(),
                false = Backend:item_in_set(Pid, Key, Value),
                
                true  = Backend:add_to_set(Pid, Key, Value),
                false = Backend:item_in_set(Pid, Key, Value),
                true

            end).

start_system_test() ->
    code:add_pathz("../apps/setref/ebin"),
    {ok, Pid} = setref_serv:start_link(),
    ?assert(is_process_alive(Pid)),
    true.
        
run_test() ->
    code:add_pathz("../apps/setref/ebin"),
    ?assertEqual([],proper:module(?MODULE,[{to_file, user}])).
