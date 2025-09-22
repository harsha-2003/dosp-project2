import node
import gleam/erlang/process.{type Subject}

pub fn start(nodes: List(Subject(node.NodeMsg))) -> Nil {
  case nodes {
    [first, ..] -> {
      // Start gossip by sending to the first node
      node.send_gossip(first)
    }
    [] -> Nil
  }
}
