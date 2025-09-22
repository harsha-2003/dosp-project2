-module(failure_model).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/failure_model.gleam").
-export([configure_disruption_profile/3, initiate_network_disruption/2, simulate_communication_disruptions/2, generate_disruption_statistics/3]).
-export_type([disruption_profile/0]).

-type disruption_profile() :: {disruption_profile, float(), float(), boolean()}.

-file("src/failure_model.gleam", 17).
-spec configure_disruption_profile(float(), float(), boolean()) -> disruption_profile().
configure_disruption_profile(
    Degradation_ratio,
    Instability_factor,
    Recovery_enabled
) ->
    {disruption_profile,
        Degradation_ratio,
        Instability_factor,
        Recovery_enabled}.

-file("src/failure_model.gleam", 25).
-spec initiate_network_disruption(
    list(gleam@erlang@process:subject(node:participant_message())),
    disruption_profile()
) -> {list(gleam@erlang@process:subject(node:participant_message())), integer()}.
initiate_network_disruption(Active_participants, Disruption_profile) ->
    Total_participants = erlang:length(Active_participants),
    Participants_to_disable = erlang:round(
        (erlang:float(Total_participants) * erlang:element(
            2,
            Disruption_profile
        ))
        / 100.0
    ),
    gleam_stdlib:println(
        <<<<"Initiating network disruption: "/utf8,
                (gleam_stdlib:float_to_string(
                    erlang:element(2, Disruption_profile)
                ))/binary>>/binary,
            "% degradation..."/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"Disabling "/utf8,
                        (erlang:integer_to_binary(Participants_to_disable))/binary>>/binary,
                    " out of "/utf8>>/binary,
                (erlang:integer_to_binary(Total_participants))/binary>>/binary,
            " network participants"/utf8>>
    ),
    Operational_participants = gleam@list:drop(
        Active_participants,
        Participants_to_disable
    ),
    Isolated_participants = gleam@list:take(
        Active_participants,
        Participants_to_disable
    ),
    gleam@list:each(Isolated_participants, fun node:isolate_participant/1),
    {Operational_participants, Participants_to_disable}.

-file("src/failure_model.gleam", 57).
-spec compute_network_connectivity_potential(integer()) -> integer().
compute_network_connectivity_potential(Participant_count) ->
    (Participant_count * 6) div 2.

-file("src/failure_model.gleam", 86).
-spec generate_communication_pairs(
    list(gleam@erlang@process:subject(node:participant_message())),
    integer()
) -> list({gleam@erlang@process:subject(node:participant_message()),
    gleam@erlang@process:subject(node:participant_message())}).
generate_communication_pairs(Participants, Count) ->
    case Participants of
        [] ->
            [];

        [_] ->
            [];

        [Primary, Secondary | Remaining] ->
            case Count of
                0 ->
                    [];

                _ ->
                    [{Primary, Secondary} |
                        generate_communication_pairs(
                            [Secondary | Remaining],
                            Count - 1
                        )]
            end
    end.

-file("src/failure_model.gleam", 62).
-spec execute_communication_disruption(
    list(gleam@erlang@process:subject(node:participant_message())),
    integer(),
    boolean()
) -> nil.
execute_communication_disruption(
    Participants,
    Disruption_count,
    Enable_recovery
) ->
    Participant_pairs = generate_communication_pairs(
        Participants,
        Disruption_count
    ),
    case Enable_recovery of
        true ->
            gleam@list:each(
                Participant_pairs,
                fun(Pair) ->
                    {Participant_a, Participant_b} = Pair,
                    node:disrupt_communication_link(
                        Participant_a,
                        Participant_b
                    ),
                    node:disrupt_communication_link(
                        Participant_b,
                        Participant_a
                    )
                end
            );

        false ->
            gleam@list:each(
                Participant_pairs,
                fun(Pair@1) ->
                    {Participant_a@1, Participant_b@1} = Pair@1,
                    node:disrupt_communication_link(
                        Participant_a@1,
                        Participant_b@1
                    ),
                    node:disrupt_communication_link(
                        Participant_b@1,
                        Participant_a@1
                    )
                end
            )
    end.

-file("src/failure_model.gleam", 42).
-spec simulate_communication_disruptions(
    list(gleam@erlang@process:subject(node:participant_message())),
    disruption_profile()
) -> integer().
simulate_communication_disruptions(Operational_participants, Disruption_profile) ->
    Total_communication_channels = compute_network_connectivity_potential(
        erlang:length(Operational_participants)
    ),
    Communication_channels_to_disrupt = erlang:round(
        erlang:float(Total_communication_channels) * erlang:element(
            3,
            Disruption_profile
        )
    ),
    case erlang:element(4, Disruption_profile) of
        true ->
            gleam_stdlib:println(
                <<"Applying transient communication disruptions..."/utf8>>
            );

        false ->
            gleam_stdlib:println(
                <<"Applying persistent communication channel failures..."/utf8>>
            )
    end,
    execute_communication_disruption(
        Operational_participants,
        Communication_channels_to_disrupt,
        erlang:element(4, Disruption_profile)
    ),
    Communication_channels_to_disrupt.

-file("src/failure_model.gleam", 100).
-spec generate_disruption_statistics(integer(), integer(), integer()) -> binary().
generate_disruption_statistics(
    Total_participants,
    Isolated_participants,
    Disrupted_channels
) ->
    Participant_isolation_rate = case Total_participants of
        0 ->
            +0.0;

        _ ->
            (case erlang:float(Total_participants) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Isolated_participants) / Gleam@denominator
            end) * 100.0
    end,
    <<<<<<<<<<<<<<"Network participant isolation: "/utf8,
                                (erlang:integer_to_binary(Isolated_participants))/binary>>/binary,
                            "/"/utf8>>/binary,
                        (erlang:integer_to_binary(Total_participants))/binary>>/binary,
                    " ("/utf8>>/binary,
                (gleam_stdlib:float_to_string(Participant_isolation_rate))/binary>>/binary,
            "%), Communication channel disruptions: "/utf8>>/binary,
        (erlang:integer_to_binary(Disrupted_channels))/binary>>.
