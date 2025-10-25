#!/bin/bash
# Test ram command on goblin in Whispering Wood

echo "Testing RAM command against goblin..."
(
  sleep 0.5
  echo "RamWarrior"
  sleep 0.5
  echo "e"  # to Moonlit Lane
  sleep 0.5
  echo "e"  # to Whispering Wood
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "ram goblin"
  sleep 1
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
