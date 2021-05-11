-module(agg_sup).

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
            id => aggregator,
            start => {aggregator, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [aggregator]
        },
        #{
            id => scheduler,
            start => {scheduler, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [scheduler]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
