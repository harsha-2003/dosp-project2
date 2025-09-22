import gleam/list
import gleam/io
import gleam/int
import node
import gleam/erlang/process.{type Subject}

pub fn build_nodes(n: Int) -> List(Subject(node.ParticipantMessage)) {
  list.range(1, n)
    |> list.map(node.initialize_network_participant)
}

pub fn connect(participants: List(Subject(node.ParticipantMessage)), network_pattern: String) -> Nil {
  case network_pattern {
    "full" -> establish_full_connectivity(participants)
    "line" -> establish_linear_connectivity(participants)
    "3D" -> establish_3d_grid_connectivity(participants)
    "imp3D" -> establish_imperfect_3d_connectivity(participants)
    _ -> {
      io.println("Unknown topology: " <> network_pattern)
      io.println("Available topologies: full, line, 3D, imp3D")
    }
  }
  io.println("Topology completely built!")
}

fn establish_full_connectivity(participants: List(Subject(node.ParticipantMessage))) -> Nil {
  list.each(participants, fn(participant) {
    let peer_connections = participants |> list.filter(fn(other) { other != participant })
    node.configure_peer_connections(participant, peer_connections)
  })
}

fn establish_linear_connectivity(participants: List(Subject(node.ParticipantMessage))) -> Nil {
  let participant_count = list.length(participants)
  let indexed_participants = list.index_map(participants, fn(participant, i) { #(i, participant) })
  
  list.each(indexed_participants, fn(item) {
    let #(i, participant) = item
    let peer_connections = case i {
      0 if participant_count > 1 -> {
        // First participant - only has right peer
        case list.drop(participants, 1) {
          [next_peer, ..] -> [next_peer]
          [] -> []
        }
      }
      i if i == participant_count - 1 && participant_count > 1 -> {
        // Last participant - only has left peer
        case list.drop(participants, participant_count - 2) |> list.take(1) {
          [prev_peer] -> [prev_peer]
          _ -> []
        }
      }
      i if i > 0 && i < participant_count - 1 -> {
        // Middle participants - have both peer connections
        let prev_connection = case list.drop(participants, i - 1) |> list.take(1) {
          [p] -> [p]
          _ -> []
        }
        let next_connection = case list.drop(participants, i + 1) |> list.take(1) {
          [n] -> [n]
          _ -> []
        }
        list.append(prev_connection, next_connection)
      }
      _ -> []
    }
    node.configure_peer_connections(participant, peer_connections)
  })
}

fn establish_3d_grid_connectivity(participants: List(Subject(node.ParticipantMessage))) -> Nil {
  let n = list.length(participants)
  // Calculate cube root to get grid dimensions
  let grid_size = compute_cube_root(n)
  
  let indexed_participants = list.index_map(participants, fn(participant, i) { #(i, participant) })
  
  list.each(indexed_participants, fn(item) {
    let #(i, participant) = item
    
    // Convert linear index to 3D coordinates
    let x = i % grid_size
    let y = { i / grid_size } % grid_size
    let z = i / { grid_size * grid_size }
    
    let peer_connections = list.filter_map(indexed_participants, fn(other_item) {
      let #(j, other_participant) = other_item
      
      // Convert other participant's linear index to 3D coordinates
      let other_x = j % grid_size
      let other_y = { j / grid_size } % grid_size
      let other_z = j / { grid_size * grid_size }
      
      // Check if it's a direct 3D neighbor (Manhattan distance = 1)
      let x_diff = int.absolute_value(x - other_x)
      let y_diff = int.absolute_value(y - other_y)
      let z_diff = int.absolute_value(z - other_z)
      
      case x_diff + y_diff + z_diff == 1 {
        True -> Ok(other_participant)
        False -> Error(Nil)
      }
    })
    
    node.configure_peer_connections(participant, peer_connections)
  })
}

fn compute_cube_root(n: Int) -> Int {
  // Simple cube root approximation for grid sizing
  case n {
    n if n <= 1 -> 1
    n if n <= 8 -> 2
    n if n <= 27 -> 3
    n if n <= 64 -> 4
    n if n <= 125 -> 5
    _ -> int.max(2, n / 10)  // Fallback for larger participant counts
  }
}

fn establish_imperfect_3d_connectivity(participants: List(Subject(node.ParticipantMessage))) -> Nil {
  let n = list.length(participants)
  let grid_size = compute_cube_root(n)
  
  let indexed_participants = list.index_map(participants, fn(participant, i) { #(i, participant) })
  
  list.each(indexed_participants, fn(item) {
    let #(i, participant) = item
    
    // Convert linear index to 3D coordinates
    let x = i % grid_size
    let y = { i / grid_size } % grid_size
    let z = i / { grid_size * grid_size }
    
    let standard_connections = list.filter_map(indexed_participants, fn(other_item) {
      let #(j, other_participant) = other_item
      
      // Convert other participant's linear index to 3D coordinates
      let other_x = j % grid_size
      let other_y = { j / grid_size } % grid_size
      let other_z = j / { grid_size * grid_size }
      
      // Check if it's a direct 3D neighbor (Manhattan distance = 1)
      let x_diff = int.absolute_value(x - other_x)
      let y_diff = int.absolute_value(y - other_y)
      let z_diff = int.absolute_value(z - other_z)
      
      case x_diff + y_diff + z_diff == 1 {
        True -> Ok(other_participant)
        False -> Error(Nil)
      }
    })
    
    // Add one random additional connection to make it "imperfect"
    let additional_connection = select_random_supplementary_connection(indexed_participants, i, standard_connections)
    let final_connections = case additional_connection {
      Ok(extra_peer) -> [extra_peer, ..standard_connections]
      Error(_) -> standard_connections
    }
    
    node.configure_peer_connections(participant, final_connections)
  })
}

fn select_random_supplementary_connection(
  all_participants: List(#(Int, Subject(node.ParticipantMessage))), 
  current_index: Int,
  existing_connections: List(Subject(node.ParticipantMessage))
) -> Result(Subject(node.ParticipantMessage), Nil) {
  let available_participants = list.filter_map(all_participants, fn(item) {
    let #(i, participant) = item
    
    // Don't select self or existing connections
    let is_self = i == current_index
    let is_existing_connection = list.contains(existing_connections, participant)
    
    case is_self || is_existing_connection {
      True -> Error(Nil)
      False -> Ok(participant)
    }
  })
  
  // For now, select the first available participant
  // In a real implementation, we'd use proper random selection
  case available_participants {
    [first_available, ..] -> Ok(first_available)
    [] -> Error(Nil)
  }
}
