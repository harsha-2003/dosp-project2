import gleam/io
import gleam/int
import coordinator

pub fn main() -> Nil {
  // Simple command-line interface
  // Change these values to test different configurations
  
  // Current configuration:
  let nodes = 50
  let topology = "imp3D"
  let algorithm = "gossip"
  
  io.println("=== Gleam Gossip Protocol Simulator ===")
  io.println("Running: " <> int.to_string(nodes) <> " nodes, " <> topology <> " topology, " <> algorithm <> " algorithm")
  io.println("")
  
  start([int.to_string(nodes), topology, algorithm])
  
  io.println("")
  io.println("To test different configurations, modify the values in main.gleam:")
  io.println("Available topologies: full, line, 3D, imp3D")
  io.println("Available algorithms: gossip, push-sum")
}

// Alternative test functions - change main() to call these instead
pub fn test_line_pushsum() -> Nil {
  start(["5", "line", "push-sum"])
}

pub fn test_large_3d_gossip() -> Nil {
  start(["3000", "3D", "gossip"])
}

pub fn test_full_pushsum() -> Nil {
  start(["5", "full", "push-sum"])
}

pub fn start(args: List(String)) -> Nil {
  case args {
    [n_str, topology, algorithm] ->
      case int.parse(n_str) {
        Ok(n) -> coordinator.run(n, topology, algorithm)
        Error(_) -> io.println("Invalid number of nodes")
      }
    _ ->
      io.println("Usage: <num_nodes> <topology: full|line|3d|imp3d> <algorithm: gossip|push-sum>")
  }
}
