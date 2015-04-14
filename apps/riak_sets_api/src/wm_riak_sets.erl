%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Yunba.io
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(wm_riak_sets).
-include_lib("types/include/types.hrl").
%% The Request data record
-type rd() :: wrq:reqdata().
-compile([{parse_transform, lager_transform}]). 
%% Used in callbacks that can halt or error. See "Halting Resources" above.
-type halt() :: {error, term()} | {halt, 200..599}.


%% The body given by streaming responses.
-type streambody() :: {iodata(), fun(() -> streambody()) | done}.
-type setkey() :: {setkey,binary()}.
-type setvalue() :: {setvalue,binary()}.

-record(state, {key   :: opt(setkey()),
		value :: opt(setvalue())}).
%% The resource's internal state
-type state() :: term().

-export([
	 init/1,
	 to_html/2
	]).
-compile(export_all).
-define(BACKEND, setref_serv).

-include_lib("webmachine/include/webmachine.hrl").
-spec init(list()) -> {ok, state()}.
init([]) ->
    {{trace, "/tmp"}, #state{}}.

-spec allowed_methods(rd(), state()) -> {[Method], rd(), state()}
      when Method :: 'GET' | 'HEAD' | 'PUT' | 'POST' |
                     'DELETE' | 'OPTIONS'.

allowed_methods(ReqData, State) ->
    {ok, Key} = get_key(ReqData),
    case get_value(ReqData) of 
	not_found ->
	        {['GET','HEAD', 'POST', 'DELETE'], 
		 ReqData,
		 State#state{
		   key   = Key,
		   value = undefined
		  }};
	{ok,Value} ->
	    {['GET','HEAD', 'DELETE'], 
	     ReqData,State#state{
		       key   = Key,
		       value = Value}}
    end.

-spec resource_exists(rd(), state()) -> {boolean() | halt(), rd(), state()}.
resource_exists(ReqData= #wm_reqdata{ method = 'GET'}, State = #state{key = Key, value = undefined}) ->
    lager:info("Resource Exits ~p", [lager:pr(State, ?MODULE)]),
    {true, ReqData, State};

resource_exists(ReqData = #wm_reqdata{method = 'GET'}, State = #state{key = Key, value = Value}) ->
    lager:info("Resource Exits ~p", [lager:pr(State, ?MODULE)]),
    lager:info("Item Exists ~p", [?BACKEND:item_in_set(?BACKEND, Key, Value)]),
    {?BACKEND:item_in_set(?BACKEND, Key, Value), ReqData, State}.


-spec to_html(rd(), term()) -> {iodata(), rd(), state()}.
to_html(ReqData, State) ->
    {"<html><body>ok</body></html>\n", ReqData, State}.

-spec to_json(rd(), term()) -> {iodata(), rd(), state()}.
to_json(ReqData,State) ->
    {"[]\n", ReqData,State}.


-spec content_types_provided(rd(), state()) -> {[{MediaType::string(), Handler::atom()}], rd(), state()}.
content_types_provided(ReqData,State) ->
    {[{"application/json", to_json}], ReqData,State}.

-spec(get_key(rd) ->maybe(setkey())).
get_key(ReqData) ->
    case wrq:path_info('key',ReqData) of
	undefined ->
	    not_found;
	Value when is_list(Value)  ->
	    {ok, {setkey, list_to_binary(Value)}}
    end.

-spec(get_value(rd) ->maybe(setvalue())).
get_value(ReqData) ->
    case wrq:path_info('value',ReqData) of
	undefined ->
	    not_found;
	Value when is_list(Value)  ->
	    {ok, {setvalue, list_to_binary(Value)}}
    end.
