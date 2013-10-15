%%% 5.4 - Event Statistics
-module(ch5_stats_handler).
-export([init/1, terminate/1, handle_event/2]).

init({data, Data}) ->
    Data;
init(Data) ->
    Data.

terminate(Data) ->
    {data, Data}.

handle_event({EventType, Id, _Description}, Data) ->
    Key = {EventType, Id},
    case lists:keyfind(Key, 1, Data) of
        false -> [{Key, 1} | Data];
        {Key, Count} ->
            lists:keyreplace(Key, 1, Data, {Key, Count + 1})
    end.

