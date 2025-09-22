import gleam/list
import gleam/int
import gleam/float
import gleam/io
import gleam/erlang/process.{type Subject}
import node

pub type DisruptionProfile {
  DisruptionProfile(
    network_degradation_ratio: Float,      // 0-100: ratio of network participants to disable initially
    link_instability_factor: Float,        // 0.0-1.0: likelihood of communication channel disruption
    enable_recovery_mechanism: Bool        // True: transient failures, False: persistent outages
  )
}


pub fn configure_disruption_profile(degradation_ratio: Float, instability_factor: Float, recovery_enabled: Bool) -> DisruptionProfile {
  DisruptionProfile(
    network_degradation_ratio: degradation_ratio,
    link_instability_factor: instability_factor,
    enable_recovery_mechanism: recovery_enabled
  )
}

pub fn initiate_network_disruption(active_participants: List(Subject(node.ParticipantMessage)), disruption_profile: DisruptionProfile) -> #(List(Subject(node.ParticipantMessage)), Int) {
  let total_participants = list.length(active_participants)
  let participants_to_disable = float.round(int.to_float(total_participants) *. disruption_profile.network_degradation_ratio /. 100.0)
  
  io.println("Initiating network disruption: " <> float.to_string(disruption_profile.network_degradation_ratio) <> "% degradation...")
  io.println("Disabling " <> int.to_string(participants_to_disable) <> " out of " <> int.to_string(total_participants) <> " network participants")
  
  // Strategic approach: eliminate initial segments to simulate localized network outages
  let operational_participants = list.drop(active_participants, participants_to_disable)
  
  // Notify affected participants of network isolation
  let isolated_participants = list.take(active_participants, participants_to_disable)
  list.each(isolated_participants, node.isolate_participant)
  
  #(operational_participants, participants_to_disable)
}

pub fn simulate_communication_disruptions(operational_participants: List(Subject(node.ParticipantMessage)), disruption_profile: DisruptionProfile) -> Int {
  let total_communication_channels = compute_network_connectivity_potential(list.length(operational_participants))
  let communication_channels_to_disrupt = float.round(int.to_float(total_communication_channels) *. disruption_profile.link_instability_factor)
  
  case disruption_profile.enable_recovery_mechanism {
    True -> io.println("Applying transient communication disruptions...")
    False -> io.println("Applying persistent communication channel failures...")
  }
  
  // Execute communication channel disruption between participant pairs
  execute_communication_disruption(operational_participants, communication_channels_to_disrupt, disruption_profile.enable_recovery_mechanism)
  
  communication_channels_to_disrupt
}

fn compute_network_connectivity_potential(participant_count: Int) -> Int {
  // Assume each network participant maintains ~6 communication channels on average
  participant_count * 6 / 2  // Halved due to bidirectional channel nature
}

fn execute_communication_disruption(participants: List(Subject(node.ParticipantMessage)), disruption_count: Int, enable_recovery: Bool) -> Nil {
  // Execute disruption pattern on communication channel pairs
  let participant_pairs = generate_communication_pairs(participants, disruption_count)
  
  case enable_recovery {
    True -> {
      // Apply transient disruptions (with recovery potential)
      list.each(participant_pairs, fn(pair) {
        let #(participant_a, participant_b) = pair
        node.disrupt_communication_link(participant_a, participant_b)
        node.disrupt_communication_link(participant_b, participant_a)
      })
    }
    False -> {
      // Apply persistent communication failures
      list.each(participant_pairs, fn(pair) {
        let #(participant_a, participant_b) = pair
        node.disrupt_communication_link(participant_a, participant_b)
        node.disrupt_communication_link(participant_b, participant_a)
      })
    }
  }
}

fn generate_communication_pairs(participants: List(Subject(node.ParticipantMessage)), count: Int) -> List(#(Subject(node.ParticipantMessage), Subject(node.ParticipantMessage))) {
  // Create participant communication pairs for disruption simulation
  case participants {
    [] -> []
    [_] -> []
    [primary, secondary, ..remaining] -> {
      case count {
        0 -> []
        _ -> [#(primary, secondary), ..generate_communication_pairs([secondary, ..remaining], count - 1)]
      }
    }
  }
}

pub fn generate_disruption_statistics(total_participants: Int, isolated_participants: Int, disrupted_channels: Int) -> String {
  let participant_isolation_rate = case total_participants {
    0 -> 0.0
    _ -> int.to_float(isolated_participants) /. int.to_float(total_participants) *. 100.0
  }
  
  "Network participant isolation: " <> int.to_string(isolated_participants) <> "/" <> int.to_string(total_participants) 
  <> " (" <> float.to_string(participant_isolation_rate) <> "%), Communication channel disruptions: " <> int.to_string(disrupted_channels)
}