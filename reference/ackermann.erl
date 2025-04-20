-module(ackermann).
-export([main/0]).

main() ->
  io:write(ackermann(4, 0)).

ackermann(0, N) ->
    N + 1;
ackermann(M, 0) ->
    ackermann(M - 1, 1);
ackermann(M, N) ->
    ackermann(M - 1, ackermann(M, N - 1)).
