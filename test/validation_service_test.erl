-module(validation_service_test).

-include_lib("eunit/include/eunit.hrl").

validation_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(ValidationUtils) ->
        [
            {"Validate valid json body", fun() -> test_valid_json_body(ValidationUtils) end},
            {"Validate malformed json body", fun() -> test_malformed_json_body(ValidationUtils) end}
        ]
    end}.

setup() ->
    {ok, JsonParser} = application:get_env(bash_gen, json_parser),
    #{json_parser => JsonParser}.

teardown(_) ->
    ok.

test_valid_json_body(ValidationUtils) ->
    {ok, Body} = file:read_file(<<"./priv/tasks-1.json">>),
    Result = validation_service:validate(Body, #{}, ValidationUtils),
    Expected = ok,
    ?assertEqual(Expected, Result).

test_malformed_json_body(ValidationUtils) ->
    Body = <<"{\n    \"tasks\":[\n        {\n            \"name\":\"task-1\",\n            \"command\":\"touch file2\"\n        },\n        {\n            \"name\":\"task-2\",\n            \"command\":\"ls -lah file2\",\n            \"requires\":[\n                \"task-3\",\n                \"task-5\"\n            ]\n        },\n        {\n            \"name\":\"task-3\",\n            \"command\":\"echo 'Hello World!' > file2\",\n            \"requires\":[\n                \"task-1\"\n            ]\n        },\n        {\n            \"name\":\"task-4\",\n            \"command\":\"pwd\",\n            \"requires\":[\n                \"task-2\",\n                \"task-3\",\n                \"task-5\"\n            ]\n        },\n        {\n            \"name\":\"task-5\",\n            \"command\":\"cat file2\",\n            \"requires\":[\n                \"task-1\",\n                \"task-3\"\n            ]\n        },\n        {\n            \"name\":\"task-6\",\n            \"command\":\"rm file2\",\n            \"requires\":[\n                \"task-4\"\n            ]\n        }\n    ]\n  \n  ">>,
    Result = validation_service:validate(Body, #{}, ValidationUtils),
    Expected = {error, invalid_json_body},
    ?assertEqual(Expected, Result).
