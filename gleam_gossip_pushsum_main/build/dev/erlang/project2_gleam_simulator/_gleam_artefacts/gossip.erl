-module(gossip).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gossip.gleam").
-export([start/1]).

-file("src/gossip.gleam", 4).
-spec start(list(gleam@erlang@process:subject(node:node_msg()))) -> nil.
start(Nodes) ->
    case Nodes of
        [First | _] ->
            node:send_gossip(First);

        [] ->
            nil
    end.
