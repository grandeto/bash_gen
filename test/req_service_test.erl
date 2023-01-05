-module(req_service_test).

-include_lib("eunit/include/eunit.hrl").

req_service_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Test read body", fun test_read_body/0},
      {"Test reply", fun test_reply/0},
      {"Test path get", fun test_path/0},
      {"Test exported path", fun test_exported_path/0},
      {"Test json decode", fun test_json_decode/0},
      {"Test json encode", fun test_json_encode/0}
     ]
    }.

setup() ->
    ok.

teardown(_) ->
    ok.

test_read_body() ->
    Body = <<"Body">>,
    Req = #{body => Body},
    ?assertEqual(Body, req_service:read_body(cowboy_req_stub, Req)).

test_reply() ->
    Headers = #{<<"content-type">> => "application/json"},
    RespBody = #{id => 1},
    Req = #{body => RespBody},
    ?assertEqual(
        #{headers => Headers, resp_body => RespBody}, 
        req_service:reply(cowboy_req_stub, 201, Headers, RespBody, Req)).

test_path() ->
    Path = <<"host/sort">>,
    Req = #{path => Path},
    ?assertEqual(
        Path, req_service:path(cowboy_req_stub, Req)).

test_exported_path() ->
    Path = <<"host/sort">>,
    Req = #{path => Path},
    ?assertEqual(<<"sort">>, req_service:exported_path(cowboy_req_stub, Req)).

test_json_decode() ->
    Body = <<"{\n  \"id\": 1\n}\n">>,
    ?assertEqual(#{<<"id">> => 1}, req_service:json_decode(jsx, Body, [to_map])).

test_json_encode() ->
    Json = #{<<"id">> => 1},
    ?assertEqual(<<"{\"id\":1}">>, req_service:json_encode(jsx, Json)).
