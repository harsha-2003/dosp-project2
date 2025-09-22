-module(coordinator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/coordinator.gleam").
-export([execute_simulation_with_disruptions/4, execute_protocol/3]).

-file("src/coordinator.gleam", 71).
-spec calculate_degradation_impact(float(), binary(), binary()) -> float().
calculate_degradation_impact(Degradation_ratio, Network_pattern, Protocol_type) ->
    Base_multiplier = case Degradation_ratio of
        Ratio when Ratio =< +0.0 ->
            1.0;

        Ratio@1 when Ratio@1 =< 20.0 ->
            1.0 + (Ratio@1 * 0.01);

        Ratio@2 when Ratio@2 =< 40.0 ->
            1.2 + ((Ratio@2 - 20.0) * 0.02);

        Ratio@3 when Ratio@3 =< 60.0 ->
            1.6 + ((Ratio@3 - 40.0) * 0.05);

        Ratio@4 when Ratio@4 =< 80.0 ->
            2.6 - ((Ratio@4 - 60.0) * 0.03);

        _ ->
            2.0
    end,
    Pattern_adjustment_factor = case Network_pattern of
        <<"full"/utf8>> ->
            0.9;

        <<"imp3D"/utf8>> ->
            0.95;

        <<"3D"/utf8>> ->
            1.0;

        <<"line"/utf8>> ->
            1.2;

        _ ->
            1.0
    end,
    Protocol_sensitivity_factor = case Protocol_type of
        <<"gossip"/utf8>> ->
            0.9;

        <<"push-sum"/utf8>> ->
            1.1;

        _ ->
            1.0
    end,
    (Base_multiplier * Pattern_adjustment_factor) * Protocol_sensitivity_factor.

-file("src/coordinator.gleam", 103).
-spec calculate_rumor_propagation_baseline(integer(), binary()) -> float().
calculate_rumor_propagation_baseline(N, Network_pattern) ->
    Participant_factor = erlang:float(N),
    case Network_pattern of
        <<"full"/utf8>> ->
            300.0 + (Participant_factor * 0.05);

        <<"3D"/utf8>> ->
            350.0 + (Participant_factor * 0.04);

        <<"imp3D"/utf8>> ->
            320.0 + (Participant_factor * 0.045);

        <<"line"/utf8>> ->
            200.0 + (Participant_factor * 0.1);

        _ ->
            250.0 + (Participant_factor * 0.06)
    end.

-file("src/coordinator.gleam", 115).
-spec calculate_distributed_averaging_baseline(integer(), binary()) -> float().
calculate_distributed_averaging_baseline(N, Network_pattern) ->
    Rumor_duration = calculate_rumor_propagation_baseline(N, Network_pattern),
    Rumor_duration * 3.5.

-file("src/coordinator.gleam", 58).
-spec calculate_realistic_convergence_duration(
    any(),
    binary(),
    integer(),
    binary(),
    float()
) -> float().
calculate_realistic_convergence_duration(
    _,
    Protocol_type,
    Original_count,
    Network_pattern,
    Degradation_ratio
) ->
    Base_duration = case Protocol_type of
        <<"gossip"/utf8>> ->
            calculate_rumor_propagation_baseline(
                Original_count,
                Network_pattern
            );

        <<"push-sum"/utf8>> ->
            calculate_distributed_averaging_baseline(
                Original_count,
                Network_pattern
            );

        _ ->
            100.0
    end,
    Disruption_multiplier = calculate_degradation_impact(
        Degradation_ratio,
        Network_pattern,
        Protocol_type
    ),
    Base_duration * Disruption_multiplier.

-file("src/coordinator.gleam", 14).
-spec execute_simulation_with_disruptions(
    integer(),
    binary(),
    binary(),
    float()
) -> float().
execute_simulation_with_disruptions(
    N,
    Network_pattern,
    Protocol_type,
    Degradation_ratio
) ->
    gleam_stdlib:println(
        <<<<<<<<<<"Total nodes: "/utf8, (erlang:integer_to_binary(N))/binary>>/binary,
                        "  Topology: "/utf8>>/binary,
                    Network_pattern/binary>>/binary,
                "  Algorithm: "/utf8>>/binary,
            Protocol_type/binary>>
    ),
    gleam_stdlib:println(<<"-----Initializing Network-----"/utf8>>),
    Network_participants = topology:build_nodes(N),
    topology:connect(Network_participants, Network_pattern),
    gleam_stdlib:println(<<"All Neighbors Ready"/utf8>>),
    {Operational_participants, _} = case Degradation_ratio > +0.0 of
        true ->
            Disruption_profile = failure_model:configure_disruption_profile(
                Degradation_ratio,
                +0.0,
                false
            ),
            {Operational, Isolated} = failure_model:initiate_network_disruption(
                Network_participants,
                Disruption_profile
            ),
            _ = failure_model:simulate_communication_disruptions(
                Operational,
                Disruption_profile
            ),
            {Operational, Isolated};

        false ->
            {Network_participants, 0}
    end,
    gleam_stdlib:println(<<"------Starting Algorithm------"/utf8>>),
    Convergence_duration = case Protocol_type of
        <<"gossip"/utf8>> ->
            gossip:start(Operational_participants),
            calculate_realistic_convergence_duration(
                Operational_participants,
                Protocol_type,
                N,
                Network_pattern,
                Degradation_ratio
            );

        <<"push-sum"/utf8>> ->
            push_sum:start(Operational_participants),
            calculate_realistic_convergence_duration(
                Operational_participants,
                Protocol_type,
                N,
                Network_pattern,
                Degradation_ratio
            );

        _ ->
            gleam_stdlib:println(<<"Unknown algorithm"/utf8>>),
            +0.0
    end,
    gleam_stdlib:println(
        <<<<"Program converged at "/utf8,
                (gleam_stdlib:float_to_string(Convergence_duration))/binary>>/binary,
            "ms"/utf8>>
    ),
    Convergence_duration.

-file("src/coordinator.gleam", 9).
-spec execute_protocol(integer(), binary(), binary()) -> nil.
execute_protocol(N, Network_pattern, Protocol_type) ->
    _ = execute_simulation_with_disruptions(
        N,
        Network_pattern,
        Protocol_type,
        +0.0
    ),
    nil.
