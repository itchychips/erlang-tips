-module(stack_server).

%-compile(nowarn_export_all).
%-compile(export_all).

-export([
         get/1,
         loop/2,
         pop/1,
         push/2,
         push_each/2,
         set/2,
         start/0,
         start/1,
         stop/1]).

start() ->
    start([]).

start(InitialValue) ->
    spawn_link(?MODULE, loop, [self(), InitialValue]).


stop(Pid) ->
    Pid ! {self(), stop},
    ok.


push_each(Pid, Values) ->
    lists:foreach(fun(X) -> ok = push(Pid, X) end, Values),
    ok.

push(Pid, Value) ->
    Pid ! {self(), push, Value},
    ok.

pop(Pid) ->
    Pid ! {self(), pop},
    receive
        {Pid, Value} ->
            Value
    after 5000 ->
        timeout
    end.

set(Pid, Value) ->
    Pid ! {self(), set, Value},
    ok.

get(Pid) ->
    Pid ! {self(), get},
    receive
        {Pid, Value} ->
            Value
    after 5000 ->
        timeout
    end.


loop(OriginatingPid, CurrentValue) ->
    receive
        {OriginatingPid, push, Value} ->
            loop(OriginatingPid, [Value|CurrentValue]);
        {OriginatingPid, pop} ->
            case CurrentValue of
                [Head|NewValue] ->
                    OriginatingPid ! {self(), Head},
                    loop(OriginatingPid, NewValue);
                [] ->
                    OriginatingPid ! {self(), empty},
                    loop(OriginatingPid, CurrentValue)
            end;
        {OriginatingPid, set, NewValue} ->
            loop(OriginatingPid, NewValue);
        {OriginatingPid, get} ->
            OriginatingPid ! {self(), CurrentValue},
            loop(OriginatingPid, CurrentValue);
        {OriginatingPid, stop} ->
            ok;
        _ ->
            loop(OriginatingPid, CurrentValue)
    end.
