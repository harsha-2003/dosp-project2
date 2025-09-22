-record(participant_configuration, {
    participant_id :: integer(),
    peer_connections :: list(gleam@erlang@process:subject(node:participant_message())),
    severed_connections :: list(gleam@erlang@process:subject(node:participant_message())),
    information_spread_counter :: integer(),
    computation_value :: float(),
    computation_weight :: float(),
    has_terminated :: boolean(),
    is_network_isolated :: boolean(),
    historical_computation_ratios :: list(float())
}).
