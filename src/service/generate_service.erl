%%%-------------------------------------------------------------------
%% @doc generate_service public API
%% @end
%%%-------------------------------------------------------------------

-module(generate_service).

-export([generate/2]).

-spec generate(list(), module()) -> {ok, binary()}.
generate(Tasks, SortService) when is_list(Tasks) ->
    {ok, Sorted} = SortService:sort(Tasks),
    Fun = fun(Task, Acc) ->
        Command = maps:get(<<"command">>, Task),
        Acc1 = list_to_binary([Acc, Command, <<"\n">>]),
        Acc1
    end,
    {ok, lists:foldl(Fun, "", Sorted)}.
