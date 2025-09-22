import gleam/erlang/process.{type Subject, send, new_subject}

pub type ParticipantMessage {
  ConfigureConnections(List(Subject(ParticipantMessage)))
  PropagateRumor
  DistributeValue(Float, Float)
  InspectCurrentState(Subject(ParticipantStatus))
  ShutdownParticipant
  // Network disruption handling messages
  IsolateFromNetwork
  RestoreToNetwork
  DisruptCommunicationWith(Subject(ParticipantMessage))  // Sever link to specific peer
  RestoreCommunicationWith(Subject(ParticipantMessage))  // Rebuild link to specific peer
}

pub type ParticipantStatus {
  ParticipantStatusReport(Int, Float, Float, Bool, Bool, List(Subject(ParticipantMessage)))  // gossip_counter, value_state, weight_state, is_terminated, is_isolated, active_connections
}

pub type ParticipantConfiguration {
  ParticipantConfiguration(
    participant_id: Int,
    peer_connections: List(Subject(ParticipantMessage)),
    severed_connections: List(Subject(ParticipantMessage)),  // Peers with disrupted communication
    information_spread_counter: Int,
    computation_value: Float,
    computation_weight: Float,
    has_terminated: Bool,
    is_network_isolated: Bool,  // Network participant isolation status
    historical_computation_ratios: List(Float)  // For distributed averaging convergence tracking
  )
}

pub fn initialize_network_participant(_participant_id: Int) -> Subject(ParticipantMessage) {
  // Initialize a communication subject for network participant
  // Note: Simplified implementation - full actor message processing would require complete OTP integration
  new_subject()
}

// Communication interface functions for participant messaging
pub fn broadcast_rumor_to(participant: Subject(ParticipantMessage)) -> Nil {
  send(participant, PropagateRumor)
}

pub fn distribute_computation_to(participant: Subject(ParticipantMessage), value: Float, weight: Float) -> Nil {
  send(participant, DistributeValue(value, weight))
}

pub fn configure_peer_connections(participant: Subject(ParticipantMessage), connections: List(Subject(ParticipantMessage))) -> Nil {
  send(participant, ConfigureConnections(connections))
}

pub fn inspect_participant_state(participant: Subject(ParticipantMessage), response_channel: Subject(ParticipantStatus)) -> Nil {
  send(participant, InspectCurrentState(response_channel))
}

pub fn shutdown_participant(participant: Subject(ParticipantMessage)) -> Nil {
  send(participant, ShutdownParticipant)
}

// Network disruption management functions
pub fn isolate_participant(participant: Subject(ParticipantMessage)) -> Nil {
  send(participant, IsolateFromNetwork)
}

pub fn restore_participant(participant: Subject(ParticipantMessage)) -> Nil {
  send(participant, RestoreToNetwork)
}

pub fn disrupt_communication_link(participant: Subject(ParticipantMessage), target_peer: Subject(ParticipantMessage)) -> Nil {
  send(participant, DisruptCommunicationWith(target_peer))
}

pub fn restore_communication_link(participant: Subject(ParticipantMessage), target_peer: Subject(ParticipantMessage)) -> Nil {
  send(participant, RestoreCommunicationWith(target_peer))
}
