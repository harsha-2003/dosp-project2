-module(node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/node.gleam").
-export([initialize_network_participant/1, broadcast_rumor_to/1, distribute_computation_to/3, configure_peer_connections/2, inspect_participant_state/2, shutdown_participant/1, isolate_participant/1, restore_participant/1, disrupt_communication_link/2, restore_communication_link/2]).
-export_type([participant_message/0, participant_status/0, participant_configuration/0]).

-type participant_message() :: {configure_connections,
        list(gleam@erlang@process:subject(participant_message()))} |
    propagate_rumor |
    {distribute_value, float(), float()} |
    {inspect_current_state, gleam@erlang@process:subject(participant_status())} |
    shutdown_participant |
    isolate_from_network |
    restore_to_network |
    {disrupt_communication_with,
        gleam@erlang@process:subject(participant_message())} |
    {restore_communication_with,
        gleam@erlang@process:subject(participant_message())}.

-type participant_status() :: {participant_status_report,
        integer(),
        float(),
        float(),
        boolean(),
        boolean(),
        list(gleam@erlang@process:subject(participant_message()))}.

-type participant_configuration() :: {participant_configuration,
        integer(),
        list(gleam@erlang@process:subject(participant_message())),
        list(gleam@erlang@process:subject(participant_message())),
        integer(),
        float(),
        float(),
        boolean(),
        boolean(),
        list(float())}.

-file("src/node.gleam", 34).
-spec initialize_network_participant(integer()) -> gleam@erlang@process:subject(participant_message()).
initialize_network_participant(_) ->
    gleam@erlang@process:new_subject().

-file("src/node.gleam", 41).
-spec broadcast_rumor_to(gleam@erlang@process:subject(participant_message())) -> nil.
broadcast_rumor_to(Participant) ->
    gleam@erlang@process:send(Participant, propagate_rumor).

-file("src/node.gleam", 45).
-spec distribute_computation_to(
    gleam@erlang@process:subject(participant_message()),
    float(),
    float()
) -> nil.
distribute_computation_to(Participant, Value, Weight) ->
    gleam@erlang@process:send(Participant, {distribute_value, Value, Weight}).

-file("src/node.gleam", 49).
-spec configure_peer_connections(
    gleam@erlang@process:subject(participant_message()),
    list(gleam@erlang@process:subject(participant_message()))
) -> nil.
configure_peer_connections(Participant, Connections) ->
    gleam@erlang@process:send(Participant, {configure_connections, Connections}).

-file("src/node.gleam", 53).
-spec inspect_participant_state(
    gleam@erlang@process:subject(participant_message()),
    gleam@erlang@process:subject(participant_status())
) -> nil.
inspect_participant_state(Participant, Response_channel) ->
    gleam@erlang@process:send(
        Participant,
        {inspect_current_state, Response_channel}
    ).

-file("src/node.gleam", 57).
-spec shutdown_participant(gleam@erlang@process:subject(participant_message())) -> nil.
shutdown_participant(Participant) ->
    gleam@erlang@process:send(Participant, shutdown_participant).

-file("src/node.gleam", 62).
-spec isolate_participant(gleam@erlang@process:subject(participant_message())) -> nil.
isolate_participant(Participant) ->
    gleam@erlang@process:send(Participant, isolate_from_network).

-file("src/node.gleam", 66).
-spec restore_participant(gleam@erlang@process:subject(participant_message())) -> nil.
restore_participant(Participant) ->
    gleam@erlang@process:send(Participant, restore_to_network).

-file("src/node.gleam", 70).
-spec disrupt_communication_link(
    gleam@erlang@process:subject(participant_message()),
    gleam@erlang@process:subject(participant_message())
) -> nil.
disrupt_communication_link(Participant, Target_peer) ->
    gleam@erlang@process:send(
        Participant,
        {disrupt_communication_with, Target_peer}
    ).

-file("src/node.gleam", 74).
-spec restore_communication_link(
    gleam@erlang@process:subject(participant_message()),
    gleam@erlang@process:subject(participant_message())
) -> nil.
restore_communication_link(Participant, Target_peer) ->
    gleam@erlang@process:send(
        Participant,
        {restore_communication_with, Target_peer}
    ).
