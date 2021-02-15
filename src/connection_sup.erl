-module(connection_sup).
-define(ROUTE_1, "http://localhost:8000/tweets/1").
-define(ROUTE_2, "http://localhost:8000/tweets/2").
-behaviour(supervisor).


%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, 
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => connection_1,
            start => {connection, start_link, [{connection_1, ?ROUTE_1}]},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [connection]
        },
        #{
            id => connection_2,
            start => {connection, start_link, [{connection_2, ?ROUTE_2}]},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [connection]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.


