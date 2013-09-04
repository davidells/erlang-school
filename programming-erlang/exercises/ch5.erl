-module(ch5).
-export([my_db_start/0, my_db_stop/0, my_db_fetch/0, my_db_write/2, my_db_delete/1, my_db_read/1, my_db_match/1, freq_start/0, freq_stop/0, freq_allocate/0, freq_deallocate/1]).
-export([loop/2]).

% 5.1

% Process skeleton

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.
     
loop(Handler, State) ->
    receive
        {request, From, Msg} ->
            {Reply, NewState} = Handler(Msg, State),
            reply(From, Reply),
            loop(Handler, NewState);
        {stop, From} ->
            reply(From, ok)
    end.

request(Name, Message) ->
    Name ! {request, self(), Message},
    receive {reply, Reply} -> Reply end.

reply(To, Reply) ->
    To ! {reply, Reply}.


% my_db client

my_db_start() ->
    InitialDb = ch3:db2_new(),
    register(my_db, spawn(ch5, loop, [fun my_db_handle/2, InitialDb])),
    ok.

my_db_stop() ->
    stop(my_db).

my_db_fetch() ->
    request(my_db, fetch).

my_db_write(Key, Element) ->
    request(my_db, {write, Key, Element}).

my_db_delete(Key) ->
    request(my_db, {delete, Key}).

my_db_read(Key) ->
    request(my_db, {read, Key}).

my_db_match(Element) ->
    request(my_db, {match, Element}).


% my_db server

my_db_handle(fetch, Db) ->
    {Db, Db};

my_db_handle({write, Key, Element}, Db) ->
    {ok, ch3:db2_write(Key, Element, Db)};

my_db_handle({delete, Key}, Db) ->
    {ok, ch3:db2_delete(Key, Db)};

my_db_handle({read, Key}, Db) ->
    {ch3:db2_read(Key, Db), Db};

my_db_handle({match, Element}, Db) ->
    {ch3:db2_match(Element, Db), Db}.




% 5.2

% frequencies client

freq_start() ->
    Frequencies = {[10,11,12,13,14,15],[]},
    register(frequency, spawn(ch5, loop, [fun freq_handle/2, Frequencies])),
    ok.

freq_stop() ->
    stop(frequency).

freq_allocate() ->
    request(frequency, {allocate, self()}).

freq_deallocate(Freq) ->
    request(frequency, {deallocate, Freq}).


% frequencies server

freq_handle({allocate, _Pid}, {[], Allocated}) ->
    {{error, no_frequency}, {[], Allocated}};

freq_handle({allocate, Pid}, {[Freq|Free], Allocated}) ->
    {{ok, Freq}, {Free, [{Freq, Pid}|Allocated]}};

freq_handle({deallocate, Freq, Pid}, {Free, Allocated}) ->
    %{ok, {[Freq|Free], lists:keydelete(Freq, 1, Allocated)}}.
    {ok, {[Freq|Free], 
          lists:filter(
            fun({F,P}) -> F =/= Freq orelse P =/= Pid end,
            Allocated)}}.
                       
