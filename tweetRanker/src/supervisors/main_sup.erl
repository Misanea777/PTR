-module(main_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    ets:new(sent_dict,[set, public, named_table]),
    SupervisorSpecification = #{
        strategy => one_for_all, 
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => sink,
            start => {sink, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [sink]
        },
        #{
            id => agg_sup,
            start => {agg_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [agg_sup]
        },
        #{
            id => dict_getter,
            start => {dict_getter, start_link, []},
            restart => temporary,
            shutdown => infinity,
            type => worker,
            modules => [dict_getter]
        },
        #{
            id => hashtag_ranker,
            start => {hashtag_ranker, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [hashtag_ranker]
        },
        #{
            id => dynamic_supervisor,
            start => {dynamic_supervisor, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [dynamic_supervisor]
        },
        #{
            id => auto_scaler,
            start => {auto_scaler, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [auto_scaler]
        },
        #{
            id => router,
            start => {router, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [router]
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
