-module(erlsqlmigrate_utils).
-export([get_timestamp/0]).

%% @spec () -> integer()
%% @doc Return the current timestamp via erlang:now().
get_timestamp() ->
    {Megaseconds,Seconds,Microseconds} = erlang:now(),
    (Megaseconds*1000000+Seconds)*1000000+Microseconds.
