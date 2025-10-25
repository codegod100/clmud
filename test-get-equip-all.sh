#!/bin/bash
# Test 'get all' and 'equip all'

echo "=== Testing GET ALL and EQUIP ALL ==="

(
  sleep 0.5
  echo "Collector"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "attack bandit"
  sleep 1
  echo "attack bandit"
  sleep 1
  echo "attack bandit"
  sleep 1
  echo "attack bandit"
  sleep 1
  echo "look"
  sleep 0.5
  echo "get all"
  sleep 1
  echo "inventory"
  sleep 0.5
  echo "equip all"
  sleep 1
  echo "status"
  sleep 1
  echo "quit"
) | nc localhost 4000
