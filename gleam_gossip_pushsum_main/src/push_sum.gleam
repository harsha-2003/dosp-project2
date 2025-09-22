import node
import gleam/erlang/process.{type Subject}

pub fn start(nodes: List(Subject(node.NodeMsg))) -> Nil {
  case nodes {
    [first, ..] -> {
      // Start push-sum by sending initial values to the first node
      node.send_pushsum(first, 0.0, 0.0)
    }
    [] -> Nil
  }
}
