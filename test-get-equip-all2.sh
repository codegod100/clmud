#!/bin/bash
# Test 'get all' and 'equip all' with actual loot

echo "=== Testing GET ALL and EQUIP ALL (with loot) ==="

(
  sleep 0.5
  echo "Warrior"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "cast fireball goblin"
  sleep 1
  echo "cast fireball wolf"
  sleep 1
  echo "cast fireball wolf"
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
