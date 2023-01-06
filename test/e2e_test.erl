-module(e2e_test).

-include_lib("eunit/include/eunit.hrl").

sort_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
        [
            {"Test /sort", fun test_sort_endpoint/0},
            {"Test /generate", fun test_generate_endpoint/0}
        ]
    }.

setup() ->
    App = bash_gen,
    ok = application:set_env(App, req_processor, cowboy_req),
    {ok, _} = application:ensure_all_started(App),
    ok.

teardown(_) ->
    application:set_env(bash_gen, req_processor, cowboy_req_stub),
    ok.

test_sort_endpoint() ->
    inets:start(),
    {ok, Task} = file:read_file(<<"./priv/tasks-1.json">>),
    Url = "http://localhost:4000/sort/",
    {ok, {{_, Status, _}, _, Result}} =
        httpc:request(post, {Url, [{"Content-Type", "application/json"}], "application/json", Task}, [], []),
    Expected1 = 201,
    Expected2 = "[{\"command\":\"touch file1\",\"name\":\"task-1\"},{\"command\":\"echo 'Hello World!' > file1\",\"name\":\"task-3\"},{\"command\":\"cat file1\",\"name\":\"task-2\"},{\"command\":\"rm file1\",\"name\":\"task-4\"}]",
    ?assertEqual(Expected1, Status),
    ?assertEqual(Expected2, Result).

test_generate_endpoint() ->
    Resp = os:cmd("curl -v -H 'Content-Type: application/json' \"http://localhost:4000/generate\" -d @./priv/tasks-2.json"),
    Result = string:str(Resp, "touch file2\necho 'Hello World!' > file2\ncat file2\nls -lah file2\npwd\nrm file2\n") > 0,
    Expected = true,
    ?assertEqual(Expected, Result).
