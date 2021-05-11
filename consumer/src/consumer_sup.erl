%%%-------------------------------------------------------------------
%% @doc consumer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(consumer_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
        id => tcp_client,
        start => {tcp_client, start, [4020]},
        restart => permanent,
        shutdown => infinity,
        modules => [tcp_client]
        }
],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
