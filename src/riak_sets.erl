%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(riak_sets).

-export([start/0]).

start() ->
    io:format("Starting riak sets~n"),
    application:ensure_all_started(riak_sets).
