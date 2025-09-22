-module(topology).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/topology.gleam").
-export([build_nodes/1, connect/2]).

-file("src/topology.gleam", 7).
-spec build_nodes(integer()) -> list(gleam@erlang@process:subject(node:participant_message())).
build_nodes(N) ->
    _pipe = gleam@list:range(1, N),
    gleam@list:map(_pipe, fun node:initialize_network_participant/1).

-file("src/topology.gleam", 26).
-spec establish_full_connectivity(
    list(gleam@erlang@process:subject(node:participant_message()))
) -> nil.
establish_full_connectivity(Participants) ->
    gleam@list:each(
        Participants,
        fun(Participant) ->
            Peer_connections = begin
                _pipe = Participants,
                gleam@list:filter(_pipe, fun(Other) -> Other /= Participant end)
            end,
            node:configure_peer_connections(Participant, Peer_connections)
        end
    ).

-file("src/topology.gleam", 33).
-spec establish_linear_connectivity(
    list(gleam@erlang@process:subject(node:participant_message()))
) -> nil.
establish_linear_connectivity(Participants) ->
    Participant_count = erlang:length(Participants),
    Indexed_participants = gleam@list:index_map(
        Participants,
        fun(Participant, I) -> {I, Participant} end
    ),
    gleam@list:each(
        Indexed_participants,
        fun(Item) ->
            {I@1, Participant@1} = Item,
            Peer_connections = case I@1 of
                0 when Participant_count > 1 ->
                    case gleam@list:drop(Participants, 1) of
                        [Next_peer | _] ->
                            [Next_peer];

                        [] ->
                            []
                    end;

                I@2 when (I@2 =:= (Participant_count - 1)) andalso (Participant_count > 1) ->
                    case begin
                        _pipe = gleam@list:drop(
                            Participants,
                            Participant_count - 2
                        ),
                        gleam@list:take(_pipe, 1)
                    end of
                        [Prev_peer] ->
                            [Prev_peer];

                        _ ->
                            []
                    end;

                I@3 when (I@3 > 0) andalso (I@3 < (Participant_count - 1)) ->
                    Prev_connection = case begin
                        _pipe@1 = gleam@list:drop(Participants, I@3 - 1),
                        gleam@list:take(_pipe@1, 1)
                    end of
                        [P] ->
                            [P];

                        _ ->
                            []
                    end,
                    Next_connection = case begin
                        _pipe@2 = gleam@list:drop(Participants, I@3 + 1),
                        gleam@list:take(_pipe@2, 1)
                    end of
                        [N] ->
                            [N];

                        _ ->
                            []
                    end,
                    lists:append(Prev_connection, Next_connection);

                _ ->
                    []
            end,
            node:configure_peer_connections(Participant@1, Peer_connections)
        end
    ).

-file("src/topology.gleam", 110).
-spec compute_cube_root(integer()) -> integer().
compute_cube_root(N) ->
    case N of
        N@1 when N@1 =< 1 ->
            1;

        N@2 when N@2 =< 8 ->
            2;

        N@3 when N@3 =< 27 ->
            3;

        N@4 when N@4 =< 64 ->
            4;

        N@5 when N@5 =< 125 ->
            5;

        _ ->
            gleam@int:max(2, N div 10)
    end.

-file("src/topology.gleam", 72).
-spec establish_3d_grid_connectivity(
    list(gleam@erlang@process:subject(node:participant_message()))
) -> nil.
establish_3d_grid_connectivity(Participants) ->
    N = erlang:length(Participants),
    Grid_size = compute_cube_root(N),
    Indexed_participants = gleam@list:index_map(
        Participants,
        fun(Participant, I) -> {I, Participant} end
    ),
    gleam@list:each(
        Indexed_participants,
        fun(Item) ->
            {I@1, Participant@1} = Item,
            X = case Grid_size of
                0 -> 0;
                Gleam@denominator -> I@1 rem Gleam@denominator
            end,
            Y = case Grid_size of
                0 -> 0;
                Gleam@denominator@2 -> (case Grid_size of
                    0 -> 0;
                    Gleam@denominator@1 -> I@1 div Gleam@denominator@1
                end) rem Gleam@denominator@2
            end,
            Z = case (Grid_size * Grid_size) of
                0 -> 0;
                Gleam@denominator@3 -> I@1 div Gleam@denominator@3
            end,
            Peer_connections = gleam@list:filter_map(
                Indexed_participants,
                fun(Other_item) ->
                    {J, Other_participant} = Other_item,
                    Other_x = case Grid_size of
                        0 -> 0;
                        Gleam@denominator@4 -> J rem Gleam@denominator@4
                    end,
                    Other_y = case Grid_size of
                        0 -> 0;
                        Gleam@denominator@6 -> (case Grid_size of
                            0 -> 0;
                            Gleam@denominator@5 -> J div Gleam@denominator@5
                        end) rem Gleam@denominator@6
                    end,
                    Other_z = case (Grid_size * Grid_size) of
                        0 -> 0;
                        Gleam@denominator@7 -> J div Gleam@denominator@7
                    end,
                    X_diff = gleam@int:absolute_value(X - Other_x),
                    Y_diff = gleam@int:absolute_value(Y - Other_y),
                    Z_diff = gleam@int:absolute_value(Z - Other_z),
                    case ((X_diff + Y_diff) + Z_diff) =:= 1 of
                        true ->
                            {ok, Other_participant};

                        false ->
                            {error, nil}
                    end
                end
            ),
            node:configure_peer_connections(Participant@1, Peer_connections)
        end
    ).

-file("src/topology.gleam", 166).
-spec select_random_supplementary_connection(
    list({integer(), gleam@erlang@process:subject(node:participant_message())}),
    integer(),
    list(gleam@erlang@process:subject(node:participant_message()))
) -> {ok, gleam@erlang@process:subject(node:participant_message())} |
    {error, nil}.
select_random_supplementary_connection(
    All_participants,
    Current_index,
    Existing_connections
) ->
    Available_participants = gleam@list:filter_map(
        All_participants,
        fun(Item) ->
            {I, Participant} = Item,
            Is_self = I =:= Current_index,
            Is_existing_connection = gleam@list:contains(
                Existing_connections,
                Participant
            ),
            case Is_self orelse Is_existing_connection of
                true ->
                    {error, nil};

                false ->
                    {ok, Participant}
            end
        end
    ),
    case Available_participants of
        [First_available | _] ->
            {ok, First_available};

        [] ->
            {error, nil}
    end.

-file("src/topology.gleam", 122).
-spec establish_imperfect_3d_connectivity(
    list(gleam@erlang@process:subject(node:participant_message()))
) -> nil.
establish_imperfect_3d_connectivity(Participants) ->
    N = erlang:length(Participants),
    Grid_size = compute_cube_root(N),
    Indexed_participants = gleam@list:index_map(
        Participants,
        fun(Participant, I) -> {I, Participant} end
    ),
    gleam@list:each(
        Indexed_participants,
        fun(Item) ->
            {I@1, Participant@1} = Item,
            X = case Grid_size of
                0 -> 0;
                Gleam@denominator -> I@1 rem Gleam@denominator
            end,
            Y = case Grid_size of
                0 -> 0;
                Gleam@denominator@2 -> (case Grid_size of
                    0 -> 0;
                    Gleam@denominator@1 -> I@1 div Gleam@denominator@1
                end) rem Gleam@denominator@2
            end,
            Z = case (Grid_size * Grid_size) of
                0 -> 0;
                Gleam@denominator@3 -> I@1 div Gleam@denominator@3
            end,
            Standard_connections = gleam@list:filter_map(
                Indexed_participants,
                fun(Other_item) ->
                    {J, Other_participant} = Other_item,
                    Other_x = case Grid_size of
                        0 -> 0;
                        Gleam@denominator@4 -> J rem Gleam@denominator@4
                    end,
                    Other_y = case Grid_size of
                        0 -> 0;
                        Gleam@denominator@6 -> (case Grid_size of
                            0 -> 0;
                            Gleam@denominator@5 -> J div Gleam@denominator@5
                        end) rem Gleam@denominator@6
                    end,
                    Other_z = case (Grid_size * Grid_size) of
                        0 -> 0;
                        Gleam@denominator@7 -> J div Gleam@denominator@7
                    end,
                    X_diff = gleam@int:absolute_value(X - Other_x),
                    Y_diff = gleam@int:absolute_value(Y - Other_y),
                    Z_diff = gleam@int:absolute_value(Z - Other_z),
                    case ((X_diff + Y_diff) + Z_diff) =:= 1 of
                        true ->
                            {ok, Other_participant};

                        false ->
                            {error, nil}
                    end
                end
            ),
            Additional_connection = select_random_supplementary_connection(
                Indexed_participants,
                I@1,
                Standard_connections
            ),
            Final_connections = case Additional_connection of
                {ok, Extra_peer} ->
                    [Extra_peer | Standard_connections];

                {error, _} ->
                    Standard_connections
            end,
            node:configure_peer_connections(Participant@1, Final_connections)
        end
    ).

-file("src/topology.gleam", 12).
-spec connect(
    list(gleam@erlang@process:subject(node:participant_message())),
    binary()
) -> nil.
connect(Participants, Network_pattern) ->
    case Network_pattern of
        <<"full"/utf8>> ->
            establish_full_connectivity(Participants);

        <<"line"/utf8>> ->
            establish_linear_connectivity(Participants);

        <<"3D"/utf8>> ->
            establish_3d_grid_connectivity(Participants);

        <<"imp3D"/utf8>> ->
            establish_imperfect_3d_connectivity(Participants);

        _ ->
            gleam_stdlib:println(
                <<"Unknown topology: "/utf8, Network_pattern/binary>>
            ),
            gleam_stdlib:println(
                <<"Available topologies: full, line, 3D, imp3D"/utf8>>
            )
    end,
    gleam_stdlib:println(<<"Topology completely built!"/utf8>>).
