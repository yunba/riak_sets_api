%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Yunba.io
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(wm_riak_sets).
-record(state, {key   :: opt(setkey()),
		value :: opt(setvalue())}).
%% The resource's internal state
-type state() :: term().

-export([
	 init/1,
	 to_html/2
	]).
-compile(export_all).

-include("api.hrl").

-spec init(list()) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.
%    {{trace, "/tmp"}, #state{}}.

-spec allowed_methods(rd(), state()) -> {[Method], rd(), state()}
      when Method :: 'GET' | 'HEAD' | 'PUT' | 'POST' |
                     'DELETE' | 'OPTIONS'.

allowed_methods(ReqData, State) ->
    {ok, Key} = get_key(ReqData),
    case get_value(ReqData) of 
	not_found ->
	        {['GET','HEAD', 'DELETE'], 
		 ReqData,
		 State#state{
		   key   = Key,
		   value = undefined
		  }};
	{ok,Value} ->
	    {['GET','HEAD', 'POST', 'DELETE'], 
	     ReqData,State#state{
		       key   = Key,
		       value = Value}}
    end.

-spec resource_exists(rd(), state()) -> {boolean() | halt(), rd(), state()}.
resource_exists(ReqData= #wm_reqdata{ method = 'GET'}, State = #state{key = Key, value = undefined}) ->
    {true, ReqData, State};

resource_exists(ReqData = #wm_reqdata{method = 'GET'}, State = #state{key = Key, value = Value}) ->
    {riak_sets:item_in_set( Key, Value), ReqData, State};

resource_exists(ReqData = #wm_reqdata{method = 'POST'}, State = #state{key = Key, value = Value}) ->
    riak_sets:add_to_set( Key, Value),
    {true, ReqData,State};

resource_exists(ReqData = #wm_reqdata{method = 'DELETE'}, State) ->
    {true, ReqData,State}.


-spec to_html(rd(), term()) -> {iodata(), rd(), state()}.
to_html(ReqData, State) ->
    {"<html><body>ok</body></html>\n", ReqData, State}.


-spec to_json(rd(), term()) -> {iodata(), rd(), state()}.
to_json(ReqData,State) ->
    {"[]\n", ReqData,State}.


-spec content_types_provided(rd(), state()) -> {[{MediaType::string(), Handler::atom()}], rd(), state()}.
content_types_provided(ReqData,State) ->
    {[{"application/json", to_json}], ReqData,State}.



post_is_create(ReqData,State) ->
    {false, ReqData,State}.

process_post(ReqData,State) ->
    {true,ReqData,State}.
    

-spec delete_resource(rd(), state()) -> {boolean() | halt(), rd(), state()}.
delete_resource(ReqData, State = #state{key = Key, value = Value}) ->
    riak_sets:remove_from_set( Key, Value),
    {true, ReqData,State}.


-spec(get_key(rd()) ->maybe(setkey())).
get_key(ReqData) ->
    case wrq:path_info('key',ReqData) of
	undefined ->
	    not_found;
	Value when is_list(Value)  ->
	    {ok, {setkey, list_to_binary(Value)}}
    end.

-spec(get_value(rd()) ->maybe(setvalue())).
get_value(ReqData) ->
    case wrq:path_info('value',ReqData) of
	undefined ->
	    not_found;
	Value when is_list(Value)  ->
	    {ok, {setvalue, list_to_binary(Value)}}
    end.
