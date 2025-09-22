-record(state, {
    id :: integer(),
    neighbors :: list(gleam@erlang@process:subject(node:node_msg())),
    failed_neighbors :: list(gleam@erlang@process:subject(node:node_msg())),
    rumor_count :: integer(),
    s :: float(),
    w :: float(),
    terminated :: boolean(),
    failed :: boolean(),
    previous_ratios :: list(float())
}).
