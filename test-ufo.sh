#!/bin/bash
# Test UFO aerial navigation

echo "=== Testing UFO and Aerial Navigation ==="
echo ""
echo "Going to get UFO from graveyard and fly around!"
echo ""

(
  sleep 0.5
  echo "Pilot"
  sleep 0.5
  echo "nw"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "enter ufo"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "se"
  sleep 0.5
  echo "up"
  sleep 1
  echo "look"
  sleep 1
  echo "e"
  sleep 1
  echo "look"
  sleep 1
  echo "w"
  sleep 0.5
  echo "w"
  sleep 1
  echo "look"
  sleep 1
  echo "down"
  sleep 1
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
