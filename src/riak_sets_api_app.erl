-module(riak_sets_api_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    riak_sets_api_sup:start_link().

stop(_State) ->
    ok.
