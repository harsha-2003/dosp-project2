-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\main.gleam").
-export([start/1, main/0]).

-file("src\\main.gleam", 9).
-spec start(list(binary())) -> nil.
start(Args) ->
    case Args of
        [N_str, Topology, Algorithm] ->
            case gleam_stdlib:parse_int(N_str) of
                {ok, N} ->
                    coordinator:run(N, Topology, Algorithm);

                {error, _} ->
                    gleam_stdlib:println(<<"Invalid number of nodes"/utf8>>)
            end;

        _ ->
            gleam_stdlib:println(
                <<"Usage: <num_nodes> <topology: full|line|3d|imp3d> <algorithm: gossip|push-sum>"/utf8>>
            )
    end.

-file("src\\main.gleam", 5).
-spec main() -> nil.
main() ->
    start([<<"50"/utf8>>, <<"full"/utf8>>, <<"gossip"/utf8>>]).
