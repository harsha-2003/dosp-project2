import gleam/erlang/process.{type Subject, send, new_subject}

pub type NodeMsg {
  SetNeighbors(List(Subject(NodeMsg)))
  Gossip
  PushSum(Float, Float)
  QueryState(Subject(StateReply))
  Terminate
}

pub type StateReply {
  NodeStateReply(Int, Float, Float, Bool)  // rumor_count, s, w, terminated
}

pub type State {
  State(
    id: Int,
    neighbors: List(Subject(NodeMsg)),
    rumor_count: Int,
    s: Float,
    w: Float,
    terminated: Bool,
    previous_ratios: List(Float)  // For push-sum convergence detection
  )
}

pub fn start_node(_id: Int) -> Subject(NodeMsg) {
  // For now, create a simple subject
  // TODO: Implement proper actor with message loop when OTP API is clearer
  new_subject()
}



// Helper functions for sending messages to actors
pub fn send_gossip(node: Subject(NodeMsg)) -> Nil {
  send(node, Gossip)
}

pub fn send_pushsum(node: Subject(NodeMsg), s: Float, w: Float) -> Nil {
  send(node, PushSum(s, w))
}

pub fn set_neighbors(node: Subject(NodeMsg), neighbors: List(Subject(NodeMsg))) -> Nil {
  send(node, SetNeighbors(neighbors))
}

pub fn query_state(node: Subject(NodeMsg), reply_to: Subject(StateReply)) -> Nil {
  send(node, QueryState(reply_to))
}

pub fn terminate_node(node: Subject(NodeMsg)) -> Nil {
  send(node, Terminate)
}
