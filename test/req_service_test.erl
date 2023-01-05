-module(req_service_test).

-include_lib("eunit/include/eunit.hrl").

req_service_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun({ReqProcessor, JsonParser}) ->
        [
            {"Test read body", fun() -> test_read_body(ReqProcessor) end},
            {"Test reply", fun() -> test_reply(ReqProcessor) end},
            {"Test path get", fun() -> test_path(ReqProcessor) end},
            {"Test exported path", fun() -> test_exported_path(ReqProcessor) end},
            {"Test json decode", fun() -> test_json_decode(JsonParser) end},
            {"Test json encode", fun() -> test_json_encode(JsonParser) end}
        ]
    end}.

setup() ->
    App = bash_gen,
    {ok, ReqProcessor} = application:get_env(App, req_processor),
    {ok, JsonParser} = application:get_env(App, json_parser),
    {ReqProcessor, JsonParser}.

teardown(_) ->
    ok.

test_read_body(ReqProcessor) ->
    Body = <<"Body">>,
    Req = #{body => Body},
    Result = req_service:read_body(ReqProcessor, Req),
    Expected = Body,
    ?assertEqual(Expected, Result).

test_reply(ReqProcessor) ->
    Headers = #{<<"content-type">> => "application/json"},
    RespBody = #{id => 1},
    Req = #{body => RespBody},
    Result = req_service:reply(ReqProcessor, 201, Headers, RespBody, Req),
    Expected = #{headers => Headers, resp_body => RespBody},
    ?assertEqual(Expected, Result).

test_path(ReqProcessor) ->
    Path = <<"host/sort">>,
    Req = #{path => Path},
    Result = req_service:path(ReqProcessor, Req),
    Expected = Path,
    ?assertEqual(Expected, Result).

test_exported_path(ReqProcessor) ->
    Path = <<"host/sort">>,
    Req = #{path => Path},
    Result = req_service:exported_path(ReqProcessor, Req),
    Expected = <<"sort">>,
    ?assertEqual(Expected, Result).

test_json_decode(JsonParser) ->
    Body = <<"{\n  \"id\": 1\n}\n">>,
    Result = req_service:json_decode(JsonParser, Body, [to_map]),
    Expected = #{<<"id">> => 1},
    ?assertEqual(Expected, Result).

test_json_encode(JsonParser) ->
    Json = #{<<"id">> => 1},
    Result = req_service:json_encode(JsonParser, Json),
    Expected = <<"{\"id\":1}">>,
    ?assertEqual(Expected, Result).
