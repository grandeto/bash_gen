%%%-------------------------------------------------------------------
%% @doc req_service public API
%% @end
%%%-------------------------------------------------------------------

-module(req_service).

-export([
    read_body/2,
    reply/5,
    path/2,
    exported_path/2,
    json_decode/3,
    json_encode/2
]).

-type processor() :: module().
-type req() :: #{atom() => any()}.
-type option() :: atom() | {atom(), any()}.
-type options() :: [option()].
-type http_status() :: non_neg_integer() | binary().
-type http_headers() :: #{binary() => iodata()}.
-type resp_body() :: iodata()
	| {sendfile, non_neg_integer(), non_neg_integer(), file:name_all()}.
-type parser() :: module().
-type json_term() :: [{binary() | atom(), json_term()}] | [{},...]
    | [json_term()] | []
    | {with_tail, json_term(), binary()}
    | #{ binary() | atom() => json_term() }
    | true | false | null
    | integer() | float()
    | binary() | atom()
    | calendar:datetime().
-type json_decoder() :: fun((binary() | end_stream | end_json) -> any()).
-type json_encoder() :: fun((json_term() | end_stream | end_json) -> any()).

-spec read_body(processor(), Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
read_body(Processor, Req) ->
    Fun = processor_func_adapter(Processor, read_body),
    Processor:Fun(Req).

-spec reply(processor(), http_status(), http_headers(), resp_body(), Req) -> Req when Req::req().
reply(Processor, Status, Headers, Body, Req) ->
    Fun = processor_func_adapter(Processor, reply),
    Processor:Fun(Status, Headers, Body, Req).

-spec path(processor(), req()) -> binary().
path(Processor, Req) ->
    Fun = processor_func_adapter(Processor, path),
    Processor:Fun(Req).

-spec exported_path(processor(), req()) -> atom() | unknown.
exported_path(Processor, Req) ->
    case binary:split(path(Processor, Req), <<"/">>, [global, trim]) of
        [_, Exported] -> Exported;
        _             -> unknown
    end.

-spec json_decode(parser(), Source::binary(), Config::options()) -> json_term() | {incomplete, json_decoder()}.
json_decode(Parser, Source, Config) ->
    Fun = json_parser_adapter(Parser, decode),
    AdaptedConfig = json_config_adapter(Parser, Config),
    Parser:Fun(Source, AdaptedConfig).

-spec json_encode(parser(), Source::json_term()) -> binary() | {incomplete, json_encoder()}.
json_encode(Parser, Source) ->
    Fun = json_parser_adapter(Parser, encode),
    Parser:Fun(Source).

%% internal functions

processor_func_adapter(cowboy_req, read_body)      -> read_body;
processor_func_adapter(cowboy_req, reply)          -> reply;
processor_func_adapter(cowboy_req, path)           -> path;
processor_func_adapter(cowboy_req_stub, read_body) -> read_body;
processor_func_adapter(cowboy_req_stub, reply)     -> reply;
processor_func_adapter(cowboy_req_stub, path)      -> path.

json_parser_adapter(jsx, decode) -> decode;
json_parser_adapter(jsx, encode) -> encode.

json_config_adapter(jsx, [to_map]) -> [return_maps].
