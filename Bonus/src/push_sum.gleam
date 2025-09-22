import node
import gleam/erlang/process.{type Subject}

pub fn start(participants: List(Subject(node.ParticipantMessage))) -> Nil {
  case participants {
    [initial_participant, ..] -> {
      // Initiate distributed averaging by transmitting initial values to the first participant
      node.distribute_computation_to(initial_participant, 0.0, 0.0)
    }
    [] -> Nil
  }
}
