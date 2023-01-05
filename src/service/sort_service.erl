-module(sort_service).

-export([
    sort/1
]).

%% @doc Sort a list of multiple bash cli tasks
-spec sort(list()) -> {ok, list()}.
sort(Data) when is_list(Data) ->
    #{sorted := Sorted, executed := _, queued := _} = sort_tasks(Data),
    {ok, lists:reverse(Sorted)}.

%% internal functions

%% @doc Init the Tasks Sorting Algorithm
-spec sort_tasks(list()) -> map().
sort_tasks(Tasks) ->
    Acc = #{sorted => [], executed => [], queued => #{}},
    Fun = fun(Task, Acc0) ->
        handle_tasks_sorting(Task, Acc0)
    end,
    lists:foldl(Fun, Acc, Tasks).

%% @doc Takes the Decision to sort, queue, re-execute, 
%% mark as executed or skip a given task
-spec handle_tasks_sorting(map(), map()) -> map().
handle_tasks_sorting(Task, Acc) ->
    Executed = maps:get(executed, Acc),
    TaskName = maps:get(<<"name">>, Task),
    case lists:member(TaskName, Executed) of
        true ->
            Acc;
        false ->
            case maps:get(<<"requires">>, Task, nil) of
                nil ->
                    Task1 = maps:remove(in_queue, Task),
                    Val = append_map_or_map_val_to_list_taken_from_map(Acc, sorted, Task1, nil),
                    Acc1 = maps:put(sorted, Val, Acc),
                    Val1 = append_map_or_map_val_to_list_taken_from_map(Acc1, executed, Task1, <<"name">>),
                    Acc2 = maps:put(executed, Val1, Acc1),
                    check_execute_queued_tasks(Task1, Acc2);
                _ ->
                    handle_task_requires(Task, Acc)
            end
    end.

%% @doc Checks for queued tasks and execute them if needed
-spec check_execute_queued_tasks(map(), map()) -> map().
check_execute_queued_tasks(Task, Acc) ->
    Queued = maps:get(queued, Acc),
    TaskName = maps:get(<<"name">>, Task),
    case maps:is_key(TaskName, Queued) of
        true ->
            TaskQueuedJobs = maps:get(TaskName, Queued),
            TaskQueuedJobsReversed = lists:reverse(TaskQueuedJobs),
            Queued1 = maps:remove(TaskName, Queued),
            Acc1 = maps:put(queued, Queued1, Acc),
            Fun = fun(TaskFromJob, Acc0) ->
                handle_tasks_sorting(TaskFromJob, Acc0)
            end,
            lists:foldl(Fun, Acc1, TaskQueuedJobsReversed);
        false ->
            Acc
    end.

%% @doc Get task's dependencies
-spec handle_task_requires(map(), map()) -> map().
handle_task_requires(Task, Acc) ->
    Requires = maps:get(<<"requires">>, Task),
    RequiresAcc = [],
    Fun = fun(RequiredTaskName, RequiresAcc0) ->
        Executed = maps:get(executed, Acc),
        case lists:member(RequiredTaskName, Executed) of
            true ->
                RequiresAcc0;
            false ->
                [RequiredTaskName | RequiresAcc0]
        end
    end,
    Requires1 = lists:foldl(Fun, RequiresAcc, Requires),
    case Requires1 of
        [] -> 
            Task1 = maps:remove(<<"requires">>, Task),
            handle_tasks_sorting(Task1, Acc);
        _ -> 
            {Requires2, Task1} = should_be_queued(Requires1, Task),
            case Requires2 of
                [] ->
                    Acc;
                _ -> 
                    handle_add_to_queue(Task1, Acc, Requires2)
            end
    end.

%% @doc Determinate those task dependencies that need to be queued and mark them as queued
-spec should_be_queued(list(), map()) -> {list(), map()}.
should_be_queued(Requires, Task) ->
    case maps:is_key(in_queue, Task) of
        true ->
            InQueue = maps:get(in_queue, Task),
            Acc = [InQueue, []],
            Fun = fun(RequiredTaskName, Acc0) ->
                [InQueue1, ShouldBeQueuedAcc] = Acc0,
                case lists:member(RequiredTaskName, InQueue1) of
                    true ->
                        [InQueue1, ShouldBeQueuedAcc];
                    false ->
                        InQueue2 = [RequiredTaskName | ShouldBeQueuedAcc],
                        ShouldBeQueuedAcc1 = [RequiredTaskName | ShouldBeQueuedAcc],
                        [InQueue2, ShouldBeQueuedAcc1]
                end
            end,
            [InQueue3, Requires1] = lists:foldl(Fun, Acc, Requires),
            Task1 = maps:put(in_queue, InQueue3, Task),
            {Requires1, Task1};
        false ->
            Task1 = maps:put(in_queue, Requires, Task),
            {Requires, Task1}
    end.

%% @doc Add a task to a queue
-spec handle_add_to_queue(map(), map(), list()) -> map().
handle_add_to_queue(Task, Acc, Requires) ->
    Fun = fun(RequiredTaskName, Acc0) ->
        Queued = maps:get(queued, Acc0),
        case maps:is_key(RequiredTaskName, Queued) of
            true -> 
                Val = append_map_or_map_val_to_list_taken_from_map(Queued, RequiredTaskName, Task, nil),
                Queued1 = maps:put(RequiredTaskName, Val, Queued),
                Acc1 = maps:put(queued, Queued1, Acc0),
                Acc1;
            false ->
                Queued1 = maps:put(RequiredTaskName, [Task], Queued),
                Acc1 = maps:put(queued, Queued1, Acc0),
                Acc1
        end
    end,
    lists:foldl(Fun, Acc, Requires).

%% @doc Get List that resides in Map1 and append to it Map2 or Map2 value
append_map_or_map_val_to_list_taken_from_map(Map1, Map1Key, Map2, nil) ->
    List = maps:get(Map1Key, Map1),
    [Map2 | List];
append_map_or_map_val_to_list_taken_from_map(Map1, Map1Key, Map2, Map2Key) ->
    List = maps:get(Map1Key, Map1),
    Value = maps:get(Map2Key, Map2),
    [Value | List].
