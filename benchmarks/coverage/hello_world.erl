-module(hello_world).
-export([hello/0]).

hello() ->
  io:write("Hello, World").
