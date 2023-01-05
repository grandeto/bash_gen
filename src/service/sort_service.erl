%%%-------------------------------------------------------------------
%% @doc sort_service public API
%% @end
%%%-------------------------------------------------------------------

-module(sort_service).

-export([sort/1]).

-spec sort(list()) -> {ok, map()}.
sort(Data) ->
    {ok, #{<<"Sorted">> => Data}}.
