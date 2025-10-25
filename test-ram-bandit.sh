#!/bin/bash
# Test ram against bandit with skiff (weaker vehicle, to see counter-attack)

echo "=== Testing RAM with skiff (weaker vehicle) against bandit ==="

(
  sleep 0.5
  echo "SkiffRammer"
  sleep 0.5
  echo "w"
  sleep 0.5
  echo "s"
  sleep 0.5
  echo "get skiff"
  sleep 0.5
  echo "enter skiff"
  sleep 0.5
  echo "n"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "stats"
  sleep 0.5
  echo "ram bandit"
  sleep 1
  echo "stats"
  sleep 1
  echo "ram bandit"
  sleep 1
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
