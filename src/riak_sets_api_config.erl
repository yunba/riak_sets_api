-module(riak_sets_api_config).
-compile([{parse_transform, lager_transform}]).
-export([
    dispatch/0,
    web_config/0
]).


-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    PrivDir        = code:priv_dir(riak_sets_api),
    RF             = filename:join([PrivDir, "dispatch.conf"]),
    lager:info("Config File ~s", [RF]),
    {ok, Dispatch} = file:consult(RF),
    Dispatch.

web_config() ->
    {ok, App}  = application:get_application(?MODULE),
    {ok, Ip}   = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].
