-module(sockserv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]). -ignore_xref([{start_link, 4}]).
-export([start/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/4]). -ignore_xref([{init, 4}]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-type operator() ::
    {Module :: pid(), Worker :: pid(), Echoing :: integer(), Timeout :: integer()}.

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport,
    username :: list(),
    operator :: operator()
}).

-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start() ->
    {ok, Port} = application:get_env(erl_playground, tcp_port),
    {ok, MaxConnections} = application:get_env(erl_playground, max_connections),

    TcpOptions = [
        {backlog, 100}
    ],

    {ok, _} = ranch:start_listener(
        sockserv_tcp,
        ranch_tcp,
        [{port, Port},
        {num_acceptors, 100}] ++ TcpOptions,
        sockserv,
        [none]
    ),

    ranch:set_max_connections(sockserv_tcp, MaxConnections),
    lager:info("server listening on tcp port ~p", [Port]),
    ok.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Definitions
%% ------------------------------------------------------------------

init(Ref, Socket, Transport, [_ProxyProtocol]) ->
    lager:info("sockserv init'ed ~p",[Socket]),

    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    Opts = [{packet, 2}, {packet_size, 16384}, {active, once}, {nodelay, true}],
    _ = Transport:setopts(Socket, Opts),

    {ok, [{Name,[{size,MaxSize},{max_overflow,MaxOverflow}],[]}]} = application:get_env(erl_playground, pools),
    Module = startOperatorModule(poolboy:start_link([{name, {local, Name}},
                                {worker_module, operator},
                                {size, MaxSize}, {max_overflow, MaxOverflow}])),
    State = #state{
        socket = Socket,
        transport = Transport,
        operator = {Module, undefined, 0, undefined}
    },

    gen_server:enter_loop(?MODULE, [], State).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

handle_cast(Request, State) ->
    _ = lager:notice("unknown handle_cast ~p", [Request]),
    {noreply, State}.

handle_info({tcp, _Port, <<>>}, State) ->
    _ = lager:notice("empty handle_info state: ~p", [State]),
    {noreply, State};
handle_info({tcp, _Port, Packet}, State = #state{socket = Socket}) ->
    Req = utils:open_envelope(Packet),

    {ok, {Type, Payload}} = utils:extract_payload(Req),
    NewState = process_packet({Type, Payload}, State, utils:unix_timestamp()),
    lager:notice("~p",[NewState]),
    ok = inet:setopts(Socket, [{active, once}]),

    {noreply, NewState};
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};
handle_info(Message, State) ->
    _ = lager:notice("unknown handle_info ~p, ~p", [Message, State]),
    {noreply, State}.

handle_call(Message, _From, State) ->
    _ = lager:notice("unknown handle_call ~p", [Message]),
    {noreply, State}.

terminate(normal, _State) ->
    _ = lager:info("Goodbye!"),
    ok;
terminate(Reason, _State) ->
    _ = lager:notice("No terminate for ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet({Type :: atom(), Payload :: binary()}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet({undefined,undefined}, State, _Now) ->
    _ = lager:notice("client sent invalid packet, ignoring ~p",[State]),
    State;
process_packet({create_session, UserName}, #state{socket = Socket, transport = Transport, username = undefined, operator = Operator}, _Now) ->
    _ = lager:info("create_session received from ~p", [UserName]),
    NewState = #state{
        socket = Socket,
        transport = Transport,
        username = binary_to_list(UserName),
        operator = Operator
    },
    sendResponse(create, #{create => menu}, NewState),
    NewState;
process_packet({server_message, Message}, State, _Now) ->
    Map = #{<<"1">> => weather, <<"2">> => answer, <<"3">> => echo, <<"4">> => menu},
    sendResponse(Message, Map, State),
    handleSpecialState(Message, State).


sendResponse(Message, _Map, State = #state{username = Username, operator = {_, Worker, Echo, Timestamp}})
    when Echo > 0 ->
    SecondsPassed = erlang:system_time(second) - Timestamp,
    if 
        SecondsPassed < 10 -> Response = matchResponse(gen_server:call(Worker, {Message, echo}));
        SecondsPassed >= 10 -> Response = matchResponse(stringResponse(op_disc, Username))
    end,
    sendData(Response, State),
    ok;
sendResponse(Message, Map, State = #state{username = Username}) ->
    Response = matchResponse(stringResponse(maps:get(Message, Map),Username)),
    sendData(Response, State),
    ok.

handleSpecialState(<<"3">>, #state{socket = Socket, transport = Transport, username = Username, operator = {Module, _, Echo, Timeout}}) 
    when Echo =:= 0 ->
    Operator = {Module, poolboy:checkout(Module), 1, erlang:system_time(second)},
    %% TODO poolboy operator logic
    #state{ socket = Socket,
            transport = Transport, 
            username = Username, 
            operator = Operator};
handleSpecialState(_, #state{socket = Socket, transport = Transport, username = Username, operator = {Module, Worker, Echo, _Timestamp}})
    when Echo > 2 ->
    poolboy:checkin(Module, Worker),
    Operator = {Module, undefined, 0, undefined},
    #state{ socket = Socket,
            transport = Transport, 
            username = Username, 
            operator = Operator};
handleSpecialState(_, #state{socket = Socket, transport = Transport, username = Username, operator = {Module, Worker, Echo, Timestamp}})
    when Echo > 0 ->
    SecondsPassed = erlang:system_time(second) - Timestamp,
    if 
        SecondsPassed < 10 -> Operator = {Module, Worker, Echo + 1, Timestamp};
        SecondsPassed >= 10 -> Operator = {Module, undefined, 0, undefined}
    end,
    #state{ socket = Socket,
            transport = Transport, 
            username = Username, 
            operator = Operator};
handleSpecialState(_, State) ->
    State.


stringResponse(menu, Username) ->
    T1 = <<"\nHello ">>,
    T2 = <<", this is an automatic responder:\nSend 1 to recieve the weather forecast\nSend 2 to recieve the answer to the ultimate question of Life, the Universe and Everything\nSend 3 to contact an operator\nSend 4 to repete this message">>,
    [T1, Username, T2];

stringResponse(weather, _Username) ->
    T1 = <<"\nThe weather today will be ">>,
    List = ["sunny", "cloudy", "rainy", "stormy"],
    [T1, list_to_binary(lists:nth(rand:uniform(length(List)), List))];

stringResponse(answer, _Username) ->
    <<"\nThe answer to the ultimate question of Life, the Universe and Everything is...\n42">>;

stringResponse(echo, _Username) ->
    <<"\nAnswering operator...">>;

stringResponse(op_disc, Username) ->
    T1 = <<"\nOperator timeout, disconnected from service">>,
    T2 = stringResponse(menu, Username),
    [T1, T2];

stringResponse(_, _Username) ->
    <<"\nCommand not understood">>.

%% ------------------------------------------------------------------
%% Auxiliary functions
%% ------------------------------------------------------------------

matchResponse(Fun) ->
    Req = #req{
        type = server_message,
        server_message_data = #server_message {
            message = Fun
        }
    },
    Req.

sendData(Response, #state{socket = Socket, transport = Transport}) ->
    Data = utils:add_envelope(Response),
    Transport:send(Socket,Data),
    lager:info("sending ~p", [Response]),
    ok.

startOperatorModule({ok, Module}) -> Module;
startOperatorModule({error,{already_started, Module}}) -> Module.