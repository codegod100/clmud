#!/bin/bash
# Test indoor restriction for vehicles

echo "=== Testing Indoor Restriction for Vehicles ==="

(
  sleep 0.5
  echo "CarDriver"
  sleep 0.5
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "north"
  sleep 1
  echo "look"
  sleep 0.5
  echo "south"
  sleep 0.5
  echo "south"
  sleep 1
  echo "look"
  sleep 0.5
  echo "exit"
  sleep 0.5
  echo "north"
  sleep 1
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
