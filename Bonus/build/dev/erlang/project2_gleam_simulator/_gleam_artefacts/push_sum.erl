-module(push_sum).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/push_sum.gleam").
-export([start/1]).

-file("src/push_sum.gleam", 4).
-spec start(list(gleam@erlang@process:subject(node:participant_message()))) -> nil.
start(Participants) ->
    case Participants of
        [Initial_participant | _] ->
            node:distribute_computation_to(Initial_participant, +0.0, +0.0);

        [] ->
            nil
    end.
