-module(cowboy_req_stub).

-export([
    read_body/1,
    reply/4,
    path/1
]).

read_body(Req) -> 
    maps:get(body, Req).

reply(_, Headers, Body, _) ->
    #{headers => Headers, resp_body => Body}.

path(#{path := Path}) ->
	Path.
