%%%-------------------------------------------------------------------
%% @doc msg_broker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(msg_broker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ets:new(topics,[bag, public, named_table]),
    ets:new(subscriptions,[bag, public, named_table]),
    ets:new(clients,[set, public, named_table]),

    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
            id => disk_storage,
            start => {disk_storage, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [disk_storage]
        },
        #{
            id => queue_holder,
            start => {queue_holder, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [queue_holder]
        },
        #{
            id => tcp_server,
            start => {tcp_server, start, [4020]},
            restart => permanent,
            shutdown => infinity,
            modules => [tcp_server]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.


