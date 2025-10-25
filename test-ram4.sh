#!/bin/bash
# Riverbank is south from market

(
  sleep 0.5
  echo "RamTest"
  sleep 0.5
  echo "w"
  sleep 0.5
  echo "s"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "get skiff"
  sleep 0.5
  echo "enter skiff"
  sleep 0.5
  echo "downstream"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "ram goblin"
  sleep 1
  echo "quit"
) | nc localhost 4000
