%%%-------------------------------------------------------------------
%% @doc bash_gen public API
%% @end
%%%-------------------------------------------------------------------

-module(bash_gen_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    App = bash_gen,
    Port = application:get_env(App, http_port, 5000),
    ReqService = application:get_env(App, req_service, req_service),
    {ok, ReqProcessor} = application:get_env(App, req_processor),
    {ok, JsonParser} = application:get_env(App, json_parser),
    Opts = #{
        req_service   => ReqService,
        req_processor => ReqProcessor,
        json_parser   => JsonParser
    },
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/sort", bash_gen_api, Opts},
            {"/generate", bash_gen_api, Opts}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_server, [{port, Port}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, cowboy_handler]
        }
    ),
    bash_gen_sup:start_link().

stop(_State) ->
    ok.
