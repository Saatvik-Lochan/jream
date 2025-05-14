-module(coverage).

-export([hello_world/0, func_call/0, recursive/0, fib/1, spawn/0,
         spawn_and_uncond_receive/0, spawn_and_cond_receive/1, gcd/2]).

% hello world
hello_world() ->
  io:write("Hello world").

% func call
func_call() ->
  other_func("called").

other_func(A) ->
  io:write(A).

% recursive
recursive() ->
  recursive_util(10_000_000, 0).

recursive_util(0, B) ->
  B;
recursive_util(A, B) ->
  recursive_util(A - 1, B + 2).

% fib
fib(0) ->
  0;
fib(1) ->
  1;
fib(A) ->
  fib(A - 1) + fib(A - 2).

% gcd 
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

% spawn
spawn() ->
  spawn(fun() -> hello_world() end).

% cond receive
spawn_and_uncond_receive() ->
  Pid = spawn(fun() -> uncond_receive() end),
  "Msg received" ! Pid.

uncond_receive() ->
  receive
    Msg ->
      io:write(Msg)
  end.

% uncond_receive receive
spawn_and_cond_receive(A) ->
  Pid = spawn(fun() -> cond_receive() end),
  A ! Pid.

cond_receive() ->
  receive
    42 ->
      io:write("Unlocked");
    _ ->
      io:write("Locked")
  end.
