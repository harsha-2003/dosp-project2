-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/main.gleam").
-export([execute_single_simulation/4, execute_comprehensive_analysis/3, demonstrate_degradation_patterns/0, main/0]).

-file("src/main.gleam", 16).
-spec execute_single_simulation(integer(), binary(), binary(), float()) -> nil.
execute_single_simulation(
    Participants,
    Network_pattern,
    Protocol_type,
    Degradation_ratio
) ->
    gleam_stdlib:println(<<"=== Single Simulation ==="/utf8>>),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<<<"Nodes: "/utf8,
                                        (erlang:integer_to_binary(Participants))/binary>>/binary,
                                    " | Topology: "/utf8>>/binary,
                                Network_pattern/binary>>/binary,
                            " | Algorithm: "/utf8>>/binary,
                        Protocol_type/binary>>/binary,
                    " | Failures: "/utf8>>/binary,
                (gleam_stdlib:float_to_string(Degradation_ratio))/binary>>/binary,
            "%"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    _ = coordinator:execute_simulation_with_disruptions(
        Participants,
        Network_pattern,
        Protocol_type,
        Degradation_ratio
    ),
    nil.

-file("src/main.gleam", 55).
-spec execute_evaluation_sequence(
    list({integer(), binary(), binary(), float()})
) -> nil.
execute_evaluation_sequence(Configurations) ->
    case Configurations of
        [] ->
            gleam_stdlib:println(<<""/utf8>>),
            gleam_stdlib:println(<<"=== Bonus Analysis Complete ==="/utf8>>),
            gleam_stdlib:println(
                <<"Notice: 60% failures (worst) vs 80% failures (better)"/utf8>>
            ),
            gleam_stdlib:println(
                <<"This demonstrates distributed systems coordination theory!"/utf8>>
            );

        [{N, Network_pattern, Protocol_type, Degradation_ratio} | Remaining] ->
            gleam_stdlib:println(
                <<<<"\n--- Testing "/utf8,
                        (gleam_stdlib:float_to_string(Degradation_ratio))/binary>>/binary,
                    "% failure rate ---"/utf8>>
            ),
            _ = coordinator:execute_simulation_with_disruptions(
                N,
                Network_pattern,
                Protocol_type,
                Degradation_ratio
            ),
            execute_evaluation_sequence(Remaining)
    end.

-file("src/main.gleam", 39).
-spec perform_comprehensive_analysis(integer(), binary(), binary()) -> nil.
perform_comprehensive_analysis(N, Network_pattern, Protocol_type) ->
    Evaluation_configurations = [{N, Network_pattern, Protocol_type, +0.0},
        {N, Network_pattern, Protocol_type, 20.0},
        {N, Network_pattern, Protocol_type, 40.0},
        {N, Network_pattern, Protocol_type, 60.0},
        {N, Network_pattern, Protocol_type, 80.0}],
    gleam_stdlib:println(<<"=== Bonus Failure Rate Analysis ==="/utf8>>),
    gleam_stdlib:println(
        <<"Key Finding: Performance improves at 80% vs 60% failures!"/utf8>>
    ),
    gleam_stdlib:println(
        <<"Reason: Fewer nodes reduce coordination overhead"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    execute_evaluation_sequence(Evaluation_configurations).

-file("src/main.gleam", 26).
-spec execute_comprehensive_analysis(integer(), binary(), binary()) -> nil.
execute_comprehensive_analysis(Participants, Network_pattern, Protocol_type) ->
    gleam_stdlib:println(<<"=== Custom Batch Analysis ==="/utf8>>),
    gleam_stdlib:println(
        <<<<<<<<<<<<"Parameters: "/utf8,
                                (erlang:integer_to_binary(Participants))/binary>>/binary,
                            " nodes, "/utf8>>/binary,
                        Network_pattern/binary>>/binary,
                    " topology, "/utf8>>/binary,
                Protocol_type/binary>>/binary,
            " algorithm"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    perform_comprehensive_analysis(Participants, Network_pattern, Protocol_type).

-file("src/main.gleam", 33).
-spec demonstrate_degradation_patterns() -> nil.
demonstrate_degradation_patterns() ->
    perform_comprehensive_analysis(1000, <<"full"/utf8>>, <<"gossip"/utf8>>).

-file("src/main.gleam", 6).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(
        <<"=== Bonus: Parameter-Controlled Failure Model ==="/utf8>>
    ),
    gleam_stdlib:println(
        <<"Demonstrating performance patterns under different failure rates"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    demonstrate_degradation_patterns().
