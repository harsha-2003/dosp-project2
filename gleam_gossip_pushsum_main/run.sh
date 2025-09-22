#!/usr/bin/env bash
set -e
if [ "$#" -ne 4 ]; then
  echo "Usage: ./run.sh <numNodes> <topology: full|line|3d|imp3d> <algorithm: gossip|push-sum>"
  echo "Example: ./run.sh 100 full gossip"
  exit 1
fi
NUM=$1
TOPO=$2
ALG=$3
# Start Erlang with compiled Gleam modules
erl -pa _build/default/lib/*/ebin -noshell -s main start "$NUM" "$TOPO" "$ALG" -s init stop
