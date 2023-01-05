-module(generate_service_test).

-include_lib("eunit/include/eunit.hrl").

generate_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(SortService) -> 
        [
            {"Generate command1", fun() -> test_generate_command1(SortService) end},
            {"Generate command2", fun() -> test_generate_command2(SortService) end}
        ]
    end}.

setup() ->
    application:get_env(bash_gen, sort_service, sort_service).

teardown(_) ->
    ok.

test_generate_command1(SortService) ->
    {ok, Task} = file:read_file(<<"./priv/tasks-1.json">>),
    Json = jsx:decode(Task, [return_maps]),
    Tasks = maps:get(<<"tasks">>, Json),
    {ok, Result} = generate_service:generate(Tasks, SortService),
    Expected = <<"touch file1\necho 'Hello World!' > file1\ncat file1\nrm file1\n">>,
    ?assertEqual(Expected, Result).

test_generate_command2(SortService) ->
    {ok, Task} = file:read_file(<<"./priv/tasks-2.json">>),
    Json = jsx:decode(Task, [return_maps]),
    Tasks = maps:get(<<"tasks">>, Json),
    {ok, Result} = generate_service:generate(Tasks, SortService),
    Expected = <<"touch file2\necho 'Hello World!' > file2\ncat file2\nls -lah file2\npwd\nrm file2\n">>,
    ?assertEqual(Expected, Result).
