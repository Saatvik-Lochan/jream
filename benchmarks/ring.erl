-module(ring).
-export([run/2]).

run(M, N) ->
  % Spawn N processes and connect them as a ring.
  Launcher = self(),
  Pids = [spawn(fun() -> proc(Launcher) end) || _ <- lists:seq(1, N)],
  [HPid | TPids] = Pids,
  RPids = TPids ++ [HPid],
  [Pid ! {next, Next} || {Pid, Next} <- lists:zip(Pids, RPids)],

  % Time sending messages around the ring M times.
  HPid ! M * N,
  receive
    done ->
      ok
  end,

  % Kill all the processes.
  [Pid ! quit || Pid <- Pids],
  ok.

proc(Launcher) ->
  receive
    quit ->
      void;
    {next, Pid} ->
      put(next, Pid),
      proc(Launcher);
    0 ->
      Launcher ! done,
      proc(Launcher);
    J ->
      get(next) ! J - 1,
      proc(Launcher)
  end.
