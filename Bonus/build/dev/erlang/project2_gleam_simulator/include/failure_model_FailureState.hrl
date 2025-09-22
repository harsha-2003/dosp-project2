-record(failure_state, {
    failed_nodes :: list(gleam@erlang@process:subject(node:node_msg())),
    failed_connections :: list({gleam@erlang@process:subject(node:node_msg()),
        gleam@erlang@process:subject(node:node_msg())})
}).
