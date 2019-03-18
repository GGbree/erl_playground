-module(sockclient).
-behaviour(gen_server).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref([{start_link, 4}]).
-export([connect/0, send/1, disconnect/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any()
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

start_link() ->
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

-spec connect() -> ok.
connect() ->
    gen_server:call(whereis(?SERVER), connect),
    CreateSession = #create_session {
        username = <<"TestUser">>
    },
    gen_server:cast(whereis(?SERVER), {create_session, CreateSession}),
    ok.

send(Arg) when is_integer(Arg) ->
    Payload = #server_message {
        message = integer_to_binary(Arg)
    },
    gen_server:cast(whereis(?SERVER),{send_message, Payload}),
    ok;
send(Arg) when is_list(Arg) ->
    Payload = #server_message {
        message = list_to_binary(Arg)
    },
    gen_server:cast(whereis(?SERVER),{send_message, Payload}),
    ok.

-spec disconnect() -> ok.
disconnect() ->
    gen_server:call(whereis(?SERVER), disconnect),
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_ARgs) ->
    lager:info("sockclient init'ed"),
    {ok, #state{}}.

handle_cast({send_message, Payload}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = server_message,
        server_message_data = Payload
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),

    {noreply, State};
handle_cast({create_session, CreateSession}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = create_session,
        create_session_data = CreateSession
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),

    {noreply, State};
handle_cast(Message, State) ->
    _ = lager:warning("No handle_cast for ~p", [Message]),
    {noreply, State}.

handle_info({tcp_closed, _Port}, State) ->
    {noreply, State#state{socket = undefined}};
handle_info({tcp, _Port, Packet}, State) ->
    Req = utils:open_envelope(Packet),
    State = process_packet(Req, State, utils:unix_timestamp()),
    {noreply, State};
handle_info(Message, State) ->
    _ = lager:warning("No handle_info for~p", [Message]),
    {noreply, State}.

handle_call(connect, _From, State) ->
    {ok, Host} = application:get_env(erl_playground, tcp_host),
    {ok, Addr} = inet:parse_address(Host),
    {ok, Port} = application:get_env(erl_playground, tcp_port),
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {packet, 2}]),

    {reply, normal, State#state{socket = Socket}};
handle_call(disconnect, _From, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    
    gen_tcp:shutdown(Socket, read_write),

    {reply, normal, State};
handle_call(Message, _From, State) ->
    _ = lager:warning("No handle_call for ~p", [Message]),
    {reply, normal, State}.

terminate(Reason, _State) ->
    _ = lager:notice("terminate ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    lager:notice("server sent invalid packet, ignoring"),
    State;
process_packet(#req{ type = Type } = Req, State, _Now)
    when Type =:= server_message ->
    #req{
        server_message_data = #server_message{
            message = Message
        }
    } = Req,
    _ = lager:info("server_message received: ~s", [binary_to_list(Message)]),
    State.
