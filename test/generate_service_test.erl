-module(generate_service_test).

-include_lib("eunit/include/eunit.hrl").

generate_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Generate command1", fun test_generate_command1/0},
      {"Generate command2", fun test_generate_command2/0}
     ]
    }.

setup() ->
    ok.

teardown(_) ->
    ok.

test_generate_command1() ->
    {ok, Task} = file:read_file(<<"./priv/tasks-1.json">>),
    Json = jsx:decode(Task, [return_maps]),
    Tasks = maps:get(<<"tasks">>, Json),
    {ok, Command} = generate_service:generate(Tasks, sort_service),
    Expected = <<"touch file1\necho 'Hello World!' > file1\ncat file1\nrm file1\n">>,
    ?assertEqual(Command, Expected).

test_generate_command2() ->
    {ok, Task} = file:read_file(<<"./priv/tasks-2.json">>),
    Json = jsx:decode(Task, [return_maps]),
    Tasks = maps:get(<<"tasks">>, Json),
    {ok, Command} = generate_service:generate(Tasks, sort_service),
    Expected = <<"touch file2\necho 'Hello World!' > file2\ncat file2\nls -lah file2\npwd\nrm file2\n">>,
    ?assertEqual(Command, Expected).
