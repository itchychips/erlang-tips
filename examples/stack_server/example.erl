-module(example).

-export([start/0]).

start() ->
    StackPid = stack_server:start(),
    io:format("1: ~p~n", [stack_server:get(StackPid)]),
    ok = stack_server:push_each(StackPid, [1,2,3,4,5]),
    io:format("2: ~p~n", [stack_server:get(StackPid)]),
    5 = stack_server:pop(StackPid),
    io:format("3: ~p~n", [stack_server:get(StackPid)]),
    4 = stack_server:pop(StackPid),
    io:format("4: ~p~n", [stack_server:get(StackPid)]),
    3 = stack_server:pop(StackPid),
    io:format("5: ~p~n", [stack_server:get(StackPid)]),
    ok = stack_server:push(StackPid, 99),
    io:format("6: ~p~n", [stack_server:get(StackPid)]),
    99 = stack_server:pop(StackPid),
    io:format("7: ~p~n", [stack_server:get(StackPid)]),
    2 = stack_server:pop(StackPid),
    io:format("8: ~p~n", [stack_server:get(StackPid)]),
    1 = stack_server:pop(StackPid),
    io:format("9: ~p~n", [stack_server:get(StackPid)]),
    empty = stack_server:pop(StackPid),
    io:format("10: ~p~n", [stack_server:get(StackPid)]),
    ok.
