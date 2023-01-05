-module(sort_service_test).

-include_lib("eunit/include/eunit.hrl").

sort_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
        [
            {"Sort tasks-1", fun test_sort_tasks1/0},
            {"Sort tasks-2", fun test_sort_tasks2/0}
        ]
    }.

setup() ->
    ok.

teardown(_) ->
    ok.

test_sort_tasks1() ->
    {ok, Task} = file:read_file(<<"./priv/tasks-1.json">>),
    Json = jsx:decode(Task, [return_maps]),
    Tasks = maps:get(<<"tasks">>, Json),
    {ok, Result} = sort_service:sort(Tasks),
    Expected = [
        #{<<"command">> => <<"touch file1">>,
            <<"name">> => <<"task-1">>},
        #{<<"command">> => <<"echo 'Hello World!' > file1">>, 
            <<"name">> => <<"task-3">>},
        #{<<"command">> => <<"cat file1">>, 
            <<"name">> => <<"task-2">>},
        #{<<"command">> => <<"rm file1">>, 
            <<"name">> => <<"task-4">>}
      ],
    ?assertEqual(Expected, Result).

test_sort_tasks2() ->
    {ok, Task} = file:read_file(<<"./priv/tasks-2.json">>),
    Json = jsx:decode(Task, [return_maps]),
    Tasks = maps:get(<<"tasks">>, Json),
    {ok, Result} = sort_service:sort(Tasks),
    Expected = [
        #{<<"command">> => <<"touch file2">>,
            <<"name">> => <<"task-1">>},
        #{<<"command">> => <<"echo 'Hello World!' > file2">>, 
            <<"name">> => <<"task-3">>},
        #{<<"command">> => <<"cat file2">>, 
            <<"name">> => <<"task-5">>},
        #{<<"command">> => <<"ls -lah file2">>, 
            <<"name">> => <<"task-2">>},
        #{<<"command">> => <<"pwd">>, 
            <<"name">> => <<"task-4">>},
        #{<<"command">> => <<"rm file2">>,
            <<"name">> => <<"task-6">>}
      ],
    ?assertEqual(Expected, Result).
