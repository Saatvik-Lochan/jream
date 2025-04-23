-module(ring).
-export([start/2]).

start(NumProcesses, NumMessages) ->
    First = spawn_ring(NumProcesses, self()),
    First ! {pass, NumMessages},

    receive
        done ->
            ok
    end.

%% Spawns the ring recursively
spawn_ring(1, Parent) ->
    spawn(fun() -> loop(Parent, undefined) end);
spawn_ring(N, Parent) ->
    Next = spawn_ring(N - 1, Parent),
    Pid = spawn(fun() -> loop(Parent, Next) end),
    Next ! {next, Pid},
    Pid.

%% Loop for each process: wait for 'next' or a token
loop(Parent, Next) ->
    receive
        {next, NextPid} ->
            loop(Parent, NextPid);
        {pass, 1} ->
            %% Last pass, notify the parent
            Parent ! done;
        {pass, N} ->
            %% Pass the message along the ring
            Next ! {pass, N - 1},
            loop(Parent, Next)
    end.
