#!/bin/bash

# Bonus Failure Model - CLI Input Script
# Usage: ./run_bonus.sh <nodes> <topology> <algorithm> [failure_percentage]
#        ./run_bonus.sh batch <nodes> <topology> <algorithm>

function print_usage() {
    echo ""
    echo "=== Bonus Failure Model - CLI Usage ==="
    echo ""
    echo "Single simulation:"
    echo "  ./run_bonus.sh <nodes> <topology> <algorithm>"
    echo "  ./run_bonus.sh <nodes> <topology> <algorithm> <failure_percentage>"
    echo ""
    echo "Batch analysis (all failure rates):"
    echo "  ./run_bonus.sh batch <nodes> <topology> <algorithm>"
    echo ""
    echo "Demo mode:"
    echo "  ./run_bonus.sh demo"
    echo ""
    echo "Parameters:"
    echo "  nodes           : Number of nodes (e.g., 1000)"
    echo "  topology        : full, line, 3D, imp3D"
    echo "  algorithm       : gossip, push-sum"
    echo "  failure_percentage : 0.0-100.0 (optional)"
    echo ""
    echo "Examples:"
    echo "  ./run_bonus.sh 1000 full gossip"
    echo "  ./run_bonus.sh 1000 full gossip 25.0"
    echo "  ./run_bonus.sh batch 800 3D push-sum"
    echo "  ./run_bonus.sh demo"
    echo ""
}

function update_gleam_file() {
    local mode="$1"
    local nodes="$2"
    local topology="$3"
    local algorithm="$4"
    local failure_rate="$5"
    
    # Backup original file
    cp src/project2_gleam_simulator.gleam src/project2_gleam_simulator.gleam.backup
    
    case "$mode" in
        "demo")
            cat > src/project2_gleam_simulator.gleam << 'EOF'
import main

pub fn main() {
  // Demo mode - shows full failure analysis
  main.main()
}
EOF
            ;;
        "single")
            cat > src/project2_gleam_simulator.gleam << EOF
import main

pub fn main() {
  // Single simulation with CLI inputs
  main.execute_single_simulation($nodes, "$topology", "$algorithm", $failure_rate)
}
EOF
            ;;
        "batch")
            cat > src/project2_gleam_simulator.gleam << EOF
import main

pub fn main() {
  // Batch analysis with CLI inputs
  main.execute_comprehensive_analysis($nodes, "$topology", "$algorithm")
}
EOF
            ;;
    esac
}

function restore_file() {
    if [ -f src/project2_gleam_simulator.gleam.backup ]; then
        mv src/project2_gleam_simulator.gleam.backup src/project2_gleam_simulator.gleam
    fi
}

# Trap to restore file on exit
trap restore_file EXIT

# Parse arguments
case "$#" in
    0)
        echo "Error: No arguments provided"
        print_usage
        exit 1
        ;;
    1)
        if [ "$1" = "demo" ]; then
            echo "=== Running Demo Mode ==="
            update_gleam_file "demo"
            gleam run
        elif [ "$1" = "help" ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
            print_usage
        else
            echo "Error: Invalid single argument"
            print_usage
            exit 1
        fi
        ;;
    3)
        # Single simulation without failure percentage
        nodes="$1"
        topology="$2"
        algorithm="$3"
        
        # Validate inputs
        if ! [[ "$nodes" =~ ^[0-9]+$ ]]; then
            echo "Error: nodes must be a positive integer"
            exit 1
        fi
        
        if [[ ! "$topology" =~ ^(full|line|3D|imp3D)$ ]]; then
            echo "Error: topology must be one of: full, line, 3D, imp3D"
            exit 1
        fi
        
        if [[ ! "$algorithm" =~ ^(gossip|push-sum)$ ]]; then
            echo "Error: algorithm must be one of: gossip, push-sum"
            exit 1
        fi
        
        echo "=== Running Single Simulation (No Failures) ==="
        echo "Nodes: $nodes | Topology: $topology | Algorithm: $algorithm"
        update_gleam_file "single" "$nodes" "$topology" "$algorithm" "0.0"
        gleam run
        ;;
    4)
        if [ "$1" = "batch" ]; then
            # Batch mode
            nodes="$2"
            topology="$3"
            algorithm="$4"
            
            # Validate inputs
            if ! [[ "$nodes" =~ ^[0-9]+$ ]]; then
                echo "Error: nodes must be a positive integer"
                exit 1
            fi
            
            if [[ ! "$topology" =~ ^(full|line|3D|imp3D)$ ]]; then
                echo "Error: topology must be one of: full, line, 3D, imp3D"
                exit 1
            fi
            
            if [[ ! "$algorithm" =~ ^(gossip|push-sum)$ ]]; then
                echo "Error: algorithm must be one of: gossip, push-sum"
                exit 1
            fi
            
            echo "=== Running Batch Analysis ==="
            echo "Nodes: $nodes | Topology: $topology | Algorithm: $algorithm"
            echo "Testing all failure rates: 0%, 20%, 40%, 60%, 80%"
            update_gleam_file "batch" "$nodes" "$topology" "$algorithm"
            gleam run
        else
            # Single simulation with failure percentage
            nodes="$1"
            topology="$2"
            algorithm="$3"
            failure_rate="$4"
            
            # Validate inputs
            if ! [[ "$nodes" =~ ^[0-9]+$ ]]; then
                echo "Error: nodes must be a positive integer"
                exit 1
            fi
            
            if [[ ! "$topology" =~ ^(full|line|3D|imp3D)$ ]]; then
                echo "Error: topology must be one of: full, line, 3D, imp3D"
                exit 1
            fi
            
            if [[ ! "$algorithm" =~ ^(gossip|push-sum)$ ]]; then
                echo "Error: algorithm must be one of: gossip, push-sum"
                exit 1
            fi
            
            if ! [[ "$failure_rate" =~ ^[0-9]+\.?[0-9]*$ ]]; then
                echo "Error: failure_percentage must be a number (0.0-100.0)"
                exit 1
            fi
            
            echo "=== Running Single Simulation with Failures ==="
            echo "Nodes: $nodes | Topology: $topology | Algorithm: $algorithm | Failures: $failure_rate%"
            update_gleam_file "single" "$nodes" "$topology" "$algorithm" "$failure_rate"
            gleam run
        fi
        ;;
    *)
        echo "Error: Invalid number of arguments"
        print_usage
        exit 1
        ;;
esac