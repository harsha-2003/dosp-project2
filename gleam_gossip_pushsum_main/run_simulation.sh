#!/bin/bash

# Gleam Gossip Protocol Simulator Runner
# Usage: ./run_simulation.sh <nodes> <topology> <algorithm>
# Example: ./run_simulation.sh 50 imp3D gossip

if [ $# -ne 3 ]; then
    echo "Usage: $0 <num_nodes> <topology> <algorithm>"
    echo ""
    echo "Available topologies: full, line, 3D, imp3D"
    echo "Available algorithms: gossip, push-sum"
    echo ""
    echo "Examples:"
    echo "  $0 5 line push-sum"
    echo "  $0 50 imp3D gossip"
    echo "  $0 100 full push-sum"
    echo "  $0 3000 3D gossip"
    exit 1
fi

NODES=$1
TOPOLOGY=$2
ALGORITHM=$3

echo "Setting up simulation with $NODES nodes, $TOPOLOGY topology, $ALGORITHM algorithm..."

# Create a temporary main.gleam with the specified parameters
cat > src/main_temp.gleam << EOF
import gleam/io
import gleam/int
import coordinator

pub fn main() -> Nil {
  start(["$NODES", "$TOPOLOGY", "$ALGORITHM"])
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
EOF

# Backup original main.gleam to a location outside src/
cp src/main.gleam main_backup_temp.gleam

# Replace main.gleam with the temporary version
mv src/main_temp.gleam src/main.gleam

# Run the simulation
echo ""
gleam run

# Restore original main.gleam
mv main_backup_temp.gleam src/main.gleam

echo ""
echo "Simulation completed!"