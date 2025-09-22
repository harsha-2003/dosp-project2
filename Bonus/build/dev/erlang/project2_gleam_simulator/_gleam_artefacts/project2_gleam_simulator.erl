-module(project2_gleam_simulator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\project2_gleam_simulator.gleam").
-export([main/0]).

-file("src\\project2_gleam_simulator.gleam", 3).
-spec main() -> nil.
main() ->
    main:execute_single_simulation(50, <<"full"/utf8>>, <<"gossip"/utf8>>, 25.0).
