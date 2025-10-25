#!/bin/bash
# Test that pedestrian exits show when on foot

echo "=== Testing Pedestrian Exits Display ==="
echo ""
echo "Expected: When on foot, should see NORTH (tavern) and SOUTH (garden)"
echo "Expected: When in car, should NOT see NORTH or SOUTH"
echo ""

(
  sleep 0.5
  echo "Walker"
  sleep 0.5
  echo "look"
  sleep 1
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
