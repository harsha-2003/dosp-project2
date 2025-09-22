import gleam/list
import gleam/io
import gleam/int
import node
import gleam/erlang/process.{type Subject}

pub fn build_nodes(n: Int) -> List(Subject(node.NodeMsg)) {
  list.range(1, n)
    |> list.map(node.start_node)
}

pub fn connect(nodes: List(Subject(node.NodeMsg)), topology: String) -> Nil {
  case topology {
    "full" -> connect_full(nodes)
    "line" -> connect_line(nodes)
    "3D" -> connect_3d(nodes)
    "imp3D" -> connect_imp3d(nodes)
    _ -> {
      io.println("Unknown topology: " <> topology)
      io.println("Available topologies: full, line, 3D, imp3D")
    }
  }
  io.println("Topology completely built!")
}

fn connect_full(nodes: List(Subject(node.NodeMsg))) -> Nil {
  list.each(nodes, fn(node_subject) {
    let neighbors = nodes |> list.filter(fn(other) { other != node_subject })
    node.set_neighbors(node_subject, neighbors)
  })
}

fn connect_line(nodes: List(Subject(node.NodeMsg))) -> Nil {
  let len = list.length(nodes)
  let indexed_nodes = list.index_map(nodes, fn(node_subject, i) { #(i, node_subject) })
  
  list.each(indexed_nodes, fn(item) {
    let #(i, node_subject) = item
    let neighbors = case i {
      0 if len > 1 -> {
        // First node - only has right neighbor
        case list.drop(nodes, 1) {
          [next, ..] -> [next]
          [] -> []
        }
      }
      i if i == len - 1 && len > 1 -> {
        // Last node - only has left neighbor
        case list.drop(nodes, len - 2) |> list.take(1) {
          [prev] -> [prev]
          _ -> []
        }
      }
      i if i > 0 && i < len - 1 -> {
        // Middle nodes - have both neighbors
        let prev = case list.drop(nodes, i - 1) |> list.take(1) {
          [p] -> [p]
          _ -> []
        }
        let next = case list.drop(nodes, i + 1) |> list.take(1) {
          [n] -> [n]
          _ -> []
        }
        list.append(prev, next)
      }
      _ -> []
    }
    node.set_neighbors(node_subject, neighbors)
  })
}

fn connect_3d(nodes: List(Subject(node.NodeMsg))) -> Nil {
  let n = list.length(nodes)
  // Calculate cube root to get grid dimensions
  let size = cube_root(n)
  
  let indexed_nodes = list.index_map(nodes, fn(node_subject, i) { #(i, node_subject) })
  
  list.each(indexed_nodes, fn(item) {
    let #(i, node_subject) = item
    
    // Convert linear index to 3D coordinates
    let x = i % size
    let y = { i / size } % size
    let z = i / { size * size }
    
    let neighbors = list.filter_map(indexed_nodes, fn(other_item) {
      let #(j, other_node) = other_item
      
      // Convert other node's linear index to 3D coordinates
      let other_x = j % size
      let other_y = { j / size } % size
      let other_z = j / { size * size }
      
      // Check if it's a direct 3D neighbor (Manhattan distance = 1)
      let x_diff = int.absolute_value(x - other_x)
      let y_diff = int.absolute_value(y - other_y)
      let z_diff = int.absolute_value(z - other_z)
      
      case x_diff + y_diff + z_diff == 1 {
        True -> Ok(other_node)
        False -> Error(Nil)
      }
    })
    
    node.set_neighbors(node_subject, neighbors)
  })
}

fn cube_root(n: Int) -> Int {
  // Simple cube root approximation
  case n {
    n if n <= 1 -> 1
    n if n <= 8 -> 2
    n if n <= 27 -> 3
    n if n <= 64 -> 4
    n if n <= 125 -> 5
    _ -> int.max(2, n / 10)  // Fallback for larger numbers
  }
}

fn connect_imp3d(nodes: List(Subject(node.NodeMsg))) -> Nil {
  let n = list.length(nodes)
  let size = cube_root(n)
  
  let indexed_nodes = list.index_map(nodes, fn(node_subject, i) { #(i, node_subject) })
  
  list.each(indexed_nodes, fn(item) {
    let #(i, node_subject) = item
    
    // Convert linear index to 3D coordinates
    let x = i % size
    let y = { i / size } % size
    let z = i / { size * size }
    
    let mut_neighbors = list.filter_map(indexed_nodes, fn(other_item) {
      let #(j, other_node) = other_item
      
      // Convert other node's linear index to 3D coordinates
      let other_x = j % size
      let other_y = { j / size } % size
      let other_z = j / { size * size }
      
      // Check if it's a direct 3D neighbor (Manhattan distance = 1)
      let x_diff = int.absolute_value(x - other_x)
      let y_diff = int.absolute_value(y - other_y)
      let z_diff = int.absolute_value(z - other_z)
      
      case x_diff + y_diff + z_diff == 1 {
        True -> Ok(other_node)
        False -> Error(Nil)
      }
    })
    
    // Add one random additional neighbor to make it "imperfect"
    let additional_neighbor = select_random_additional_neighbor(indexed_nodes, i, mut_neighbors)
    let final_neighbors = case additional_neighbor {
      Ok(neighbor) -> [neighbor, ..mut_neighbors]
      Error(_) -> mut_neighbors
    }
    
    node.set_neighbors(node_subject, final_neighbors)
  })
}

fn select_random_additional_neighbor(
  all_nodes: List(#(Int, Subject(node.NodeMsg))), 
  current_index: Int,
  existing_neighbors: List(Subject(node.NodeMsg))
) -> Result(Subject(node.NodeMsg), Nil) {
  let available_nodes = list.filter_map(all_nodes, fn(item) {
    let #(i, node_subject) = item
    
    // Don't select self or existing neighbors
    let is_self = i == current_index
    let is_existing_neighbor = list.contains(existing_neighbors, node_subject)
    
    case is_self || is_existing_neighbor {
      True -> Error(Nil)
      False -> Ok(node_subject)
    }
  })
  
  // For now, select the first available node
  // In a real implementation, we'd use proper random selection
  case available_nodes {
    [first, ..] -> Ok(first)
    [] -> Error(Nil)
  }
}
