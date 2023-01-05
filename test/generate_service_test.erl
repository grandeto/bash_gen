-module(generate_service_test).

-include_lib("eunit/include/eunit.hrl").

generate_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Generate data", fun test_generate_data/0}
     ]
    }.

setup() ->
    ok.

teardown(_) ->
    ok.

test_generate_data() ->
    Data = #{id => 1},
    ?assertEqual({ok, #{<<"Generated">> => Data}}, generate_service:generate(Data)).
