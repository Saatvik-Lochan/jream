-module(gray).
-export([main/0, main/1, gray/1]).

main() ->
  io:write(gray_burn(0, 0, 5000)).

main(N) ->
  io:write(gray_burn(0, 0, N)).

gray(N) -> N bxor (N bsr 1).

gray_burn(N, Acc, Limit) when N > Limit -> Acc;
gray_burn(N, Acc, Limit) ->
    gray_burn(N + 1, Acc bxor gray(N), Limit).
