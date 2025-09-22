-module(node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/node.gleam").
-export([start_node/1, send_gossip/1, send_pushsum/3, set_neighbors/2, query_state/2, terminate_node/1]).
-export_type([node_msg/0, state_reply/0, state/0]).

-type node_msg() :: {set_neighbors,
        list(gleam@erlang@process:subject(node_msg()))} |
    gossip |
    {push_sum, float(), float()} |
    {query_state, gleam@erlang@process:subject(state_reply())} |
    terminate.

-type state_reply() :: {node_state_reply,
        integer(),
        float(),
        float(),
        boolean()}.

-type state() :: {state,
        integer(),
        list(gleam@erlang@process:subject(node_msg())),
        integer(),
        float(),
        float(),
        boolean(),
        list(float())}.

-file("src/node.gleam", 27).
-spec start_node(integer()) -> gleam@erlang@process:subject(node_msg()).
start_node(_) ->
    gleam@erlang@process:new_subject().

-file("src/node.gleam", 36).
-spec send_gossip(gleam@erlang@process:subject(node_msg())) -> nil.
send_gossip(Node) ->
    gleam@erlang@process:send(Node, gossip).

-file("src/node.gleam", 40).
-spec send_pushsum(gleam@erlang@process:subject(node_msg()), float(), float()) -> nil.
send_pushsum(Node, S, W) ->
    gleam@erlang@process:send(Node, {push_sum, S, W}).

-file("src/node.gleam", 44).
-spec set_neighbors(
    gleam@erlang@process:subject(node_msg()),
    list(gleam@erlang@process:subject(node_msg()))
) -> nil.
set_neighbors(Node, Neighbors) ->
    gleam@erlang@process:send(Node, {set_neighbors, Neighbors}).

-file("src/node.gleam", 48).
-spec query_state(
    gleam@erlang@process:subject(node_msg()),
    gleam@erlang@process:subject(state_reply())
) -> nil.
query_state(Node, Reply_to) ->
    gleam@erlang@process:send(Node, {query_state, Reply_to}).

-file("src/node.gleam", 52).
-spec terminate_node(gleam@erlang@process:subject(node_msg())) -> nil.
terminate_node(Node) ->
    gleam@erlang@process:send(Node, terminate).
