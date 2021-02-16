-module(dynamic_supervisor).
-behaviour(supervisor).
-define(MIN_WORKERS, 1).

%% API
-export([start_link/0, add_workers/1, remove_workers/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => worker,
            start => {worker, start_link, []},
            restart => permanent, 
            shutdown => infinity,
            type => worker, 
            modules => [worker]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.


add_worker() ->
    {ok, Child1Pid} = supervisor:start_child(?MODULE, []),
    Child1Pid.
remove_worker(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

add_workers(0) ->
    ok;

add_workers(N) ->
    add_worker(),
    add_workers(N-1).

remove_workers(N) ->
    Workers = supervisor:which_children(?MODULE),
    remove_workers(N, Workers).

remove_workers(N, Workers) when (N == 0) or (length(Workers) == ?MIN_WORKERS) ->
    ok;

remove_workers(N, Workers) ->
    [{_Id, Pid, _Type, _Modules}|T] = Workers,
    remove_worker(Pid),
    remove_workers(N-1, T).






