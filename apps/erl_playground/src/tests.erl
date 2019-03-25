-module(tests).

-include_lib("eunit/include/eunit.hrl").

erl_playground_test_() ->
    {foreach,
        fun() ->
            error_logger:tty(false)
        end,
        fun(_) ->
            case whereis(play_test) of
                undefined -> ok;
                Pid -> pool_call(Pid, stop)
            end,
            error_logger:tty(true)
        end,
        [
            {<<"The client is able to connect and send messages">>,
                fun connection/0},
            {<<"The client is able recieve the correct messages">>,
                fun messages/0},
            {<<"The client can only send 3 messages for 10 sec to operators">>,
                fun operators/0},
            {<<"The client is sent back to the main menu when all operators are busy">>,
                fun sentback/0},
        ]
    }.

connection() ->
    %% Check basic operations.
    Pid = erl_playground_sup:start_link().
    ?assertNotEqual(undefined, Pid),
    timer:sleep(1000),
    sockclient:connect(username).

messages() ->
    ok.

operators() ->
    ok.

sentback() ->
    ok.