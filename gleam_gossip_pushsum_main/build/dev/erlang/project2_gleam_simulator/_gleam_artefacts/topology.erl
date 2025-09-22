-module(topology).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/topology.gleam").
-export([build_nodes/1, connect/2]).

-file("src/topology.gleam", 7).
-spec build_nodes(integer()) -> list(gleam@erlang@process:subject(node:node_msg())).
build_nodes(N) ->
    _pipe = gleam@list:range(1, N),
    gleam@list:map(_pipe, fun node:start_node/1).

-file("src/topology.gleam", 26).
-spec connect_full(list(gleam@erlang@process:subject(node:node_msg()))) -> nil.
connect_full(Nodes) ->
    gleam@list:each(
        Nodes,
        fun(Node_subject) ->
            Neighbors = begin
                _pipe = Nodes,
                gleam@list:filter(
                    _pipe,
                    fun(Other) -> Other /= Node_subject end
                )
            end,
            node:set_neighbors(Node_subject, Neighbors)
        end
    ).

-file("src/topology.gleam", 33).
-spec connect_line(list(gleam@erlang@process:subject(node:node_msg()))) -> nil.
connect_line(Nodes) ->
    Len = erlang:length(Nodes),
    Indexed_nodes = gleam@list:index_map(
        Nodes,
        fun(Node_subject, I) -> {I, Node_subject} end
    ),
    gleam@list:each(
        Indexed_nodes,
        fun(Item) ->
            {I@1, Node_subject@1} = Item,
            Neighbors = case I@1 of
                0 when Len > 1 ->
                    case gleam@list:drop(Nodes, 1) of
                        [Next | _] ->
                            [Next];

                        [] ->
                            []
                    end;

                I@2 when (I@2 =:= (Len - 1)) andalso (Len > 1) ->
                    case begin
                        _pipe = gleam@list:drop(Nodes, Len - 2),
                        gleam@list:take(_pipe, 1)
                    end of
                        [Prev] ->
                            [Prev];

                        _ ->
                            []
                    end;

                I@3 when (I@3 > 0) andalso (I@3 < (Len - 1)) ->
                    Prev@1 = case begin
                        _pipe@1 = gleam@list:drop(Nodes, I@3 - 1),
                        gleam@list:take(_pipe@1, 1)
                    end of
                        [P] ->
                            [P];

                        _ ->
                            []
                    end,
                    Next@1 = case begin
                        _pipe@2 = gleam@list:drop(Nodes, I@3 + 1),
                        gleam@list:take(_pipe@2, 1)
                    end of
                        [N] ->
                            [N];

                        _ ->
                            []
                    end,
                    lists:append(Prev@1, Next@1);

                _ ->
                    []
            end,
            node:set_neighbors(Node_subject@1, Neighbors)
        end
    ).

-file("src/topology.gleam", 110).
-spec cube_root(integer()) -> integer().
cube_root(N) ->
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
-spec connect_3d(list(gleam@erlang@process:subject(node:node_msg()))) -> nil.
connect_3d(Nodes) ->
    N = erlang:length(Nodes),
    Size = cube_root(N),
    Indexed_nodes = gleam@list:index_map(
        Nodes,
        fun(Node_subject, I) -> {I, Node_subject} end
    ),
    gleam@list:each(
        Indexed_nodes,
        fun(Item) ->
            {I@1, Node_subject@1} = Item,
            X = case Size of
                0 -> 0;
                Gleam@denominator -> I@1 rem Gleam@denominator
            end,
            Y = case Size of
                0 -> 0;
                Gleam@denominator@2 -> (case Size of
                    0 -> 0;
                    Gleam@denominator@1 -> I@1 div Gleam@denominator@1
                end) rem Gleam@denominator@2
            end,
            Z = case (Size * Size) of
                0 -> 0;
                Gleam@denominator@3 -> I@1 div Gleam@denominator@3
            end,
            Neighbors = gleam@list:filter_map(
                Indexed_nodes,
                fun(Other_item) ->
                    {J, Other_node} = Other_item,
                    Other_x = case Size of
                        0 -> 0;
                        Gleam@denominator@4 -> J rem Gleam@denominator@4
                    end,
                    Other_y = case Size of
                        0 -> 0;
                        Gleam@denominator@6 -> (case Size of
                            0 -> 0;
                            Gleam@denominator@5 -> J div Gleam@denominator@5
                        end) rem Gleam@denominator@6
                    end,
                    Other_z = case (Size * Size) of
                        0 -> 0;
                        Gleam@denominator@7 -> J div Gleam@denominator@7
                    end,
                    X_diff = gleam@int:absolute_value(X - Other_x),
                    Y_diff = gleam@int:absolute_value(Y - Other_y),
                    Z_diff = gleam@int:absolute_value(Z - Other_z),
                    case ((X_diff + Y_diff) + Z_diff) =:= 1 of
                        true ->
                            {ok, Other_node};

                        false ->
                            {error, nil}
                    end
                end
            ),
            node:set_neighbors(Node_subject@1, Neighbors)
        end
    ).

-file("src/topology.gleam", 166).
-spec select_random_additional_neighbor(
    list({integer(), gleam@erlang@process:subject(node:node_msg())}),
    integer(),
    list(gleam@erlang@process:subject(node:node_msg()))
) -> {ok, gleam@erlang@process:subject(node:node_msg())} | {error, nil}.
select_random_additional_neighbor(All_nodes, Current_index, Existing_neighbors) ->
    Available_nodes = gleam@list:filter_map(
        All_nodes,
        fun(Item) ->
            {I, Node_subject} = Item,
            Is_self = I =:= Current_index,
            Is_existing_neighbor = gleam@list:contains(
                Existing_neighbors,
                Node_subject
            ),
            case Is_self orelse Is_existing_neighbor of
                true ->
                    {error, nil};

                false ->
                    {ok, Node_subject}
            end
        end
    ),
    case Available_nodes of
        [First | _] ->
            {ok, First};

        [] ->
            {error, nil}
    end.

-file("src/topology.gleam", 122).
-spec connect_imp3d(list(gleam@erlang@process:subject(node:node_msg()))) -> nil.
connect_imp3d(Nodes) ->
    N = erlang:length(Nodes),
    Size = cube_root(N),
    Indexed_nodes = gleam@list:index_map(
        Nodes,
        fun(Node_subject, I) -> {I, Node_subject} end
    ),
    gleam@list:each(
        Indexed_nodes,
        fun(Item) ->
            {I@1, Node_subject@1} = Item,
            X = case Size of
                0 -> 0;
                Gleam@denominator -> I@1 rem Gleam@denominator
            end,
            Y = case Size of
                0 -> 0;
                Gleam@denominator@2 -> (case Size of
                    0 -> 0;
                    Gleam@denominator@1 -> I@1 div Gleam@denominator@1
                end) rem Gleam@denominator@2
            end,
            Z = case (Size * Size) of
                0 -> 0;
                Gleam@denominator@3 -> I@1 div Gleam@denominator@3
            end,
            Mut_neighbors = gleam@list:filter_map(
                Indexed_nodes,
                fun(Other_item) ->
                    {J, Other_node} = Other_item,
                    Other_x = case Size of
                        0 -> 0;
                        Gleam@denominator@4 -> J rem Gleam@denominator@4
                    end,
                    Other_y = case Size of
                        0 -> 0;
                        Gleam@denominator@6 -> (case Size of
                            0 -> 0;
                            Gleam@denominator@5 -> J div Gleam@denominator@5
                        end) rem Gleam@denominator@6
                    end,
                    Other_z = case (Size * Size) of
                        0 -> 0;
                        Gleam@denominator@7 -> J div Gleam@denominator@7
                    end,
                    X_diff = gleam@int:absolute_value(X - Other_x),
                    Y_diff = gleam@int:absolute_value(Y - Other_y),
                    Z_diff = gleam@int:absolute_value(Z - Other_z),
                    case ((X_diff + Y_diff) + Z_diff) =:= 1 of
                        true ->
                            {ok, Other_node};

                        false ->
                            {error, nil}
                    end
                end
            ),
            Additional_neighbor = select_random_additional_neighbor(
                Indexed_nodes,
                I@1,
                Mut_neighbors
            ),
            Final_neighbors = case Additional_neighbor of
                {ok, Neighbor} ->
                    [Neighbor | Mut_neighbors];

                {error, _} ->
                    Mut_neighbors
            end,
            node:set_neighbors(Node_subject@1, Final_neighbors)
        end
    ).

-file("src/topology.gleam", 12).
-spec connect(list(gleam@erlang@process:subject(node:node_msg())), binary()) -> nil.
connect(Nodes, Topology) ->
    case Topology of
        <<"full"/utf8>> ->
            connect_full(Nodes);

        <<"line"/utf8>> ->
            connect_line(Nodes);

        <<"3D"/utf8>> ->
            connect_3d(Nodes);

        <<"imp3D"/utf8>> ->
            connect_imp3d(Nodes);

        _ ->
            gleam_stdlib:println(<<"Unknown topology: "/utf8, Topology/binary>>),
            gleam_stdlib:println(
                <<"Available topologies: full, line, 3D, imp3D"/utf8>>
            )
    end,
    gleam_stdlib:println(<<"Topology completely built!"/utf8>>).
