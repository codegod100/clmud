#!/bin/bash
# Find the skiff and test ram

(
  sleep 0.5
  echo "TestPlayer2"
  sleep 0.5
  echo "go s"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "get eternal"
  sleep 0.5
  echo "enter eternal"
  sleep 0.5
  echo "downstream"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "ram goblin"
  sleep 1
  echo "quit"
) | nc localhost 4000
