%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Yunba.io
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(wm_riak_sets_count).




-record(state, {key   :: opt(setkey())}).
%% The resource's internal state
-type state() :: #state{}.



-include("api.hrl").
-spec init(list()) -> {ok, state()}.
init([]) ->
%    {ok, #state{}}.
    {{trace, "/tmp"}, #state{}}.
-spec allowed_methods(rd(), state()) -> {[Method], rd(), state()}
      when Method :: 'GET' | 'HEAD' | 'PUT' | 'POST' |
                     'DELETE' | 'OPTIONS'.

allowed_methods(ReqData, State) ->
    {['GET','HEAD'], ReqData,State}.


resource_exists(ReqData , State ) ->

    {true, ReqData,State}.

-spec to_json(rd(), term()) -> {iodata(), rd(), state()}.
to_json(ReqData,State) ->
    {ok, Key}    = wm_riak_sets:get_key(ReqData),

    Size = riak_sets:size(Key),
    
    {[jsx:encode(Size), "\n"], ReqData,State}.

-spec content_types_provided(rd(), state()) -> {[{MediaType::string(), Handler::atom()}], rd(), state()}.
content_types_provided(ReqData,State) ->
    {[{"application/json", to_json}], ReqData,State}.
