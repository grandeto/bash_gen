%%%-------------------------------------------------------------------
%% @doc bash_gen_api public API
%% @end
%%%-------------------------------------------------------------------

-module(bash_gen_api).

-include_lib("src/utils/logger.hrl").

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
    Action = generate,
    #{
        req_service        := ReqService, 
        req_processor      := ReqProcessor,
        json_parser        := JsonParser,
        generate_service   := GenerateService,
        sort_service       := SortService,
        validation_service := {ValidationService, ValidationOpts}
    } = State,
    {ok, Body, Req} = ReqService:read_body(ReqProcessor, Req0),
    ?log_info("~p req raw: ~p", [Action, Body]),
    case ValidationService:validate(Body, ValidationOpts, #{json_parser => JsonParser}) of
        ok ->
            Decoded = ReqService:json_decode(JsonParser, Body, [to_map]),
            ?log_info("~p req decoded: ~p", [Action, Decoded]),
            {ok, Result} = GenerateService:generate(maps:get(<<"tasks">>, Decoded), SortService),
            ?log_info("~p resp: ~p", [Action, Result]),
            {stop, ReqService:reply(ReqProcessor, 201, #{<<"content-type">> => "text/plain"}, Result, Req), State};
        {error, Reason} ->
            ?log_error("~p validation error: ~p", [Action, Reason]),
            {stop, ReqService:reply(ReqProcessor, 400, #{<<"content-type">> => "application/json"}, error_reason(Reason, ReqService, JsonParser), Req), State}
    end.

do_sort_from_json(Req0, State) ->
    Action = sort,
    ContentTypeApplicationJson = #{<<"content-type">> => "application/json"},
    #{
        req_service        := ReqService, 
        req_processor      := ReqProcessor,
        json_parser        := JsonParser,
        sort_service       := SortService,
        validation_service := {ValidationService, ValidationOpts}
    } = State,
    {ok, Body, Req} = ReqService:read_body(ReqProcessor, Req0),
    ?log_info("~p req raw: ~p", [Action, Body]),
    case ValidationService:validate(Body, ValidationOpts, #{json_parser => JsonParser}) of
        ok ->
            Decoded = ReqService:json_decode(JsonParser, Body, [to_map]),
            ?log_info("~p req decoded: ~p", [Action, Decoded]),
            {ok, Result} = SortService:sort(maps:get(<<"tasks">>, Decoded)),
            Resp = ReqService:json_encode(JsonParser, Result),
            ?log_info("~p resp: ~p", [Action, Resp]),
            {stop, ReqService:reply(ReqProcessor, 201, ContentTypeApplicationJson, Resp, Req), State};
        {error, Reason} ->
            ?log_error("~p validation error: ~p", [Action, Reason]),
            {stop, ReqService:reply(ReqProcessor, 400, ContentTypeApplicationJson, error_reason(Reason, ReqService, JsonParser), Req), State}
    end.  

%% internal functions

error_reason(Reason, ReqService, JsonParser) ->
    ReqService:json_encode(JsonParser, #{error => Reason}).
