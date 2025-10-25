#!/bin/bash
# Test ram against a tougher enemy (wolf) to see counter-attack

echo "=== Testing RAM with counter-attack (wolf has more HP) ==="

(
  sleep 0.5
  echo "WolfRammer"
  sleep 0.5
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "ram wolf"
  sleep 1
  echo "status"
  sleep 1
  echo "ram wolf"
  sleep 1
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
