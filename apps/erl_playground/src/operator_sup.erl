-module(operator_sup).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0, echo/1]).
-export([start/2, stop/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, Pools} = application:get_env(example, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
            		{worker_module, example_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

echo(Message) ->
    poolboy:transaction(operators, fun(Worker) ->
        gen_server:call(Worker, {Message, echo}, 10000)
    end).