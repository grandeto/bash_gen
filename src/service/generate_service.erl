%%%-------------------------------------------------------------------
%% @doc generate_service public API
%% @end
%%%-------------------------------------------------------------------

-module(generate_service).

-export([generate/1]).

-spec generate(list()) -> {ok, map()}.
generate(Data) ->
    {ok, #{<<"Generated">> => Data}}.
