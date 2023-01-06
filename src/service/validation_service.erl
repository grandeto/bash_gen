%%%-------------------------------------------------------------------
%% @doc validation_service public API
%% @end
%%%-------------------------------------------------------------------

-module(validation_service).

-export([
    validate/3,
    is_valid_json/1
]).

-type reason() :: atom().

-spec validate(map(), map(), map()) -> ok | {error, reason()}.
validate(Body, ValidationOpts, Utils) ->
    Validators = [is_valid_json],
    run_validators({Body, ValidationOpts, Utils}, Validators).

%% internal functions

run_validators(_, []) -> ok;
run_validators(_, {error, Reason}) -> {error, Reason};
run_validators(Args, [Validator | Validators]) ->
    case erlang:apply(validation_service, Validator, [Args]) of
        {error, Reason} ->
            run_validators(nil, {error, Reason});
        ok ->
            run_validators(Args, Validators)
    end.
    

is_valid_json({Body, _, #{json_parser := JsonParser}}) ->
    Fun = json_parser_adapter(JsonParser, is_valid_json),
    case JsonParser:Fun(Body) of
        true -> ok;
        _ -> {error, invalid_json_body}
    end.

json_parser_adapter(jsx, is_valid_json) -> is_json.

% TODO validators:
% min_task_len
% max_task_len
% is_map_body
% is_list_tasks
% are_tasks_ids_sequential_numerics
% is_list_task_requires
% are_task_requires_ids_valid
