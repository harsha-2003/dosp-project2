-module(gossip).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gossip.gleam").
-export([start/1]).

-file("src/gossip.gleam", 4).
-spec start(list(gleam@erlang@process:subject(node:participant_message()))) -> nil.
start(Participants) ->
    case Participants of
        [Initial_participant | _] ->
            node:broadcast_rumor_to(Initial_participant);

        [] ->
            nil
    end.
