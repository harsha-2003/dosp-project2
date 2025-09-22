import node
import gleam/erlang/process.{type Subject}

pub fn start(participants: List(Subject(node.ParticipantMessage))) -> Nil {
  case participants {
    [initial_participant, ..] -> {
      // Initiate rumor propagation by messaging the first participant
      node.broadcast_rumor_to(initial_participant)
    }
    [] -> Nil
  }
}
