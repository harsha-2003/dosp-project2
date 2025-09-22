-module(push_sum).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/push_sum.gleam").
-export([start/1]).

-file("src/push_sum.gleam", 4).
-spec start(list(gleam@erlang@process:subject(node:node_msg()))) -> nil.
start(Nodes) ->
    case Nodes of
        [First | _] ->
            node:send_pushsum(First, +0.0, +0.0);

        [] ->
            nil
    end.
