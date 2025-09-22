-record(state, {
    id :: integer(),
    neighbors :: list(gleam@erlang@process:subject(node:node_msg())),
    rumor_count :: integer(),
    s :: float(),
    w :: float(),
    terminated :: boolean(),
    previous_ratios :: list(float())
}).
