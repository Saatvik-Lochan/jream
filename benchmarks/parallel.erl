-module(parallel).
-export([fibp/2]).

fibp(Procs, N) ->
  Launcher = self(),
  [spawn(fun() -> fibprocess(Launcher, N) end) || _ <- lists:seq(1, Procs)],
  gather_results(Procs, 0).

gather_results(0, Acc) ->
  Acc;

gather_results(N, Acc) ->
  receive
    Result ->
      gather_results(N - 1, Acc + Result)
  end.

fibprocess(Launcher, N) ->
  Result = fib(N),
  Launcher ! Result.

fib(0) ->
  0;
fib(1) ->
  1;
fib(N) ->
  fib(N - 1) + fib(N - 2).
