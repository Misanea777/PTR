%%%-------------------------------------------------------------------
%% @doc msg_broker public API
%% @end
%%%-------------------------------------------------------------------

-module(msg_broker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    msg_broker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
