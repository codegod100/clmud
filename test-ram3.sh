#!/bin/bash
# Find the skiff and test ram - go WEST from village square

(
  sleep 0.5
  echo "TestPlayer3"
  sleep 0.5
  echo "go w"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "get skiff"
  sleep 0.5
  echo "enter skiff"
  sleep 0.5
  echo "go downstream"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "ram goblin"
  sleep 1
  echo "quit"
) | nc localhost 4000
