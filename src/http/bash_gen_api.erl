%%%-------------------------------------------------------------------
%% @doc bash_gen_api public API
%% @end
%%%-------------------------------------------------------------------

-module(bash_gen_api).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2
]).

-export([
    do_sort_from_json/2,
    do_generate_from_json/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    #{req_service := ReqService, req_processor := ReqProcessor} = State,
    POST = <<"POST">>,
    case ReqService:exported_path(ReqProcessor, Req) of
        <<"generate">> -> {[POST], Req, State};
        <<"sort">>     -> {[POST], Req, State};
        _              -> {[], Req, State}
    end.

content_types_accepted(Req, State) ->
    #{req_service := ReqService, req_processor := ReqProcessor} = State,
    ContentTypeApplicationJson = {<<"application">>, <<"json">>, []},
    Accepted =
        case ReqService:exported_path(ReqProcessor, Req) of
            <<"generate">> -> {ContentTypeApplicationJson, do_generate_from_json};
            <<"sort">>     -> {ContentTypeApplicationJson, do_sort_from_json}
        end,
    {[Accepted], Req, State}.

do_generate_from_json(Req0, State) ->
    #{
        req_service := ReqService, 
        req_processor := ReqProcessor,
        json_parser := JsonParser
    } = State,
    {ok, Body, Req} = ReqService:read_body(ReqProcessor, Req0),
    Data = ReqService:json_decode(JsonParser, Body, [to_map]),
    Resp = ReqService:json_encode(JsonParser, Data),
    {stop, ReqService:reply(ReqProcessor, 201, #{<<"content-type">> => "application/json"}, Resp, Req), State}.

do_sort_from_json(Req0, State) ->
    #{
        req_service := ReqService, 
        req_processor := ReqProcessor,
        json_parser := JsonParser
    } = State,
    {ok, Body, Req} = ReqService:read_body(ReqProcessor, Req0),
    Data = ReqService:json_decode(JsonParser, Body, [to_map]),
    Resp = ReqService:json_encode(JsonParser, Data),
    {stop, ReqService:reply(ReqProcessor, 201, #{<<"content-type">> => "application/json"}, Resp, Req), State}.   
