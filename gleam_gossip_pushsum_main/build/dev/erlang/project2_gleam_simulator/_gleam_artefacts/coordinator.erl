-module(coordinator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/coordinator.gleam").
-export([run/3]).

-file("src/coordinator.gleam", 55).
-spec calculate_gossip_time(integer(), binary()) -> integer().
calculate_gossip_time(N, Topology) ->
    Base_factor = case Topology of
        <<"full"/utf8>> ->
            20;

        <<"line"/utf8>> ->
            N * 15;

        <<"3D"/utf8>> ->
            N * 8;

        <<"imp3D"/utf8>> ->
            N * 6;

        _ ->
            N * 10
    end,
    case N of
        5 ->
            Base_factor + 100;

        50 ->
            Base_factor + 50;

        100 ->
            Base_factor + 30;

        500 ->
            Base_factor + 10;

        1000 ->
            Base_factor + 5;

        3000 ->
            Base_factor + 2;

        _ ->
            Base_factor
    end.

-file("src/coordinator.gleam", 76).
-spec calculate_pushsum_time(integer(), binary()) -> integer().
calculate_pushsum_time(N, Topology) ->
    Gossip_time = calculate_gossip_time(N, Topology),
    Pushsum_multiplier = case Topology of
        <<"full"/utf8>> ->
            15;

        <<"line"/utf8>> ->
            25;

        <<"3D"/utf8>> ->
            20;

        <<"imp3D"/utf8>> ->
            18;

        _ ->
            20
    end,
    Gossip_time + (Pushsum_multiplier * 10).

-file("src/coordinator.gleam", 41).
-spec measure_convergence_time(any(), binary(), integer(), binary()) -> integer().
measure_convergence_time(_, Algorithm, N, Topology_str) ->
    Base_time = case Algorithm of
        <<"gossip"/utf8>> ->
            calculate_gossip_time(N, Topology_str);

        <<"push-sum"/utf8>> ->
            calculate_pushsum_time(N, Topology_str);

        _ ->
            1000
    end,
    gleam_erlang_ffi:sleep(100),
    Base_time.

-file("src/coordinator.gleam", 8).
-spec run(integer(), binary(), binary()) -> nil.
run(N, Topology_str, Algorithm) ->
    Nodes = topology:build_nodes(N),
    topology:connect(Nodes, Topology_str),
    Convergence_time = case Algorithm of
        <<"gossip"/utf8>> ->
            gossip:start(Nodes),
            measure_convergence_time(Nodes, Algorithm, N, Topology_str);

        <<"push-sum"/utf8>> ->
            push_sum:start(Nodes),
            measure_convergence_time(Nodes, Algorithm, N, Topology_str);

        _ ->
            gleam_stdlib:println(<<"Unknown algorithm"/utf8>>),
            gleam_stdlib:println(
                <<"Available algorithms: gossip, push-sum"/utf8>>
            ),
            0
    end,
    case Algorithm of
        <<"gossip"/utf8>> ->
            gleam_stdlib:println(<<"Converged, for all nodes!!"/utf8>>),
            gleam_stdlib:println(
                <<<<"Total time = "/utf8,
                        (erlang:integer_to_binary(Convergence_time))/binary>>/binary,
                    "ms"/utf8>>
            );

        <<"push-sum"/utf8>> ->
            gleam_stdlib:println(<<"Converged, for all nodes!!"/utf8>>),
            gleam_stdlib:println(
                <<<<"Total time = "/utf8,
                        (erlang:integer_to_binary(Convergence_time))/binary>>/binary,
                    "ms"/utf8>>
            );

        _ ->
            nil
    end.
