-define(BACKEND, riak_sets).
-include_lib("types/include/types.hrl").
%% The Request data record
-type rd() :: wrq:reqdata().
-compile([{parse_transform, lager_transform}]). 
%% Used in callbacks that can halt or error. See "Halting Resources" above.
-type halt() :: {error, term()} | {halt, 200..599}.




-type setkey()   :: {setkey,binary()}.
-type setvalue() :: {setvalue,binary()}.

-include_lib("webmachine/include/webmachine.hrl").
-compile(export_all).
