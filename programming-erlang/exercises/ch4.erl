-module(ch4).
-export([echo_start/0, echo_print/1, echo_stop/0, echo_server_loop/0,
         ring_go/3, ring_init/5]).

% 4.1

echo_start() ->
    register(echo, spawn(ch4, echo_server_loop, [])),
    ok.

echo_print(Msg) ->
    echo ! {print, Msg},
    ok.

echo_stop() ->
    echo ! stop,
    ok.

echo_server_loop() ->
    receive
        {print, Msg} ->
            io:format("~w~n", [Msg]),
            echo_server_loop();
        stop ->
            true
    end.


% 4.2

ring_go(MessageCount, ProcessCount, Message) ->
    spawn(ch4, ring_init, [empty, empty, ProcessCount, MessageCount, Message]),
    ok.


ring_init(HeadPid, PreviousPid, ProcessNumber, MessageCount, Message) ->

    io:format("ring_init: ~w: PreviousPid = ~w, ProcessNumber = ~w~n",
              [self(), PreviousPid, ProcessNumber]),

    IsFirstProcess = PreviousPid =:= empty,
    IsLastProcess = ProcessNumber =:= 1,

    NewHeadPid = 
        case IsFirstProcess of
            true -> self();
            false -> HeadPid
        end,

    NextPid = 
        case IsLastProcess of
            true -> HeadPid;
            false -> 
                spawn(ch4, ring_init, 
                      [NewHeadPid, self(), 
                       ProcessNumber - 1, MessageCount, Message])
        end,

    case IsLastProcess of
        true -> HeadPid ! {msg, self(), Message, MessageCount};
        false -> ok
    end,

    ring_receive(NextPid, IsLastProcess).


ring_receive(NextPid, IsLastProcess) ->
    receive
        {msg, _From, ReceivedMessage, MessageRound} ->
            io:format("ring_receive: ~w: received message = ~w, round = ~w~n", 
                      [self(), ReceivedMessage, MessageRound]),

            NextMessage = 
                case {IsLastProcess, MessageRound} of
                    {true, 1} -> stop;
                    {true, _} -> {msg, self(), ReceivedMessage, MessageRound - 1};
                    {_, _} -> {msg, self(), ReceivedMessage, MessageRound}
                end,
            
            NextPid ! NextMessage,
            ring_receive(NextPid, IsLastProcess);

        stop ->
            io:format("ring_receive: ~w: received stop message~n", [self()]),
            NextPid ! stop,
            true
    end.

