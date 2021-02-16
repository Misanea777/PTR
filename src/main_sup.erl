-module(main_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_all, 
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => auto_scaler,
            start => {auto_scaler, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [auto_scaler]
        },
        #{
            id => counter,
            start => {counter, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [counter]
        },
        #{
            id => connection_sup,
            start => {connection_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [connection_sup]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
