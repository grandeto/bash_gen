-module(sort_service_test).

-include_lib("eunit/include/eunit.hrl").

sort_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Sort data", fun test_sort_data/0}
     ]
    }.

setup() ->
    ok.

teardown(_) ->
    ok.

test_sort_data() ->
    Data = #{id => 1},
    ?assertEqual({ok, #{<<"Sorted">> => Data}}, sort_service:sort(Data)).
