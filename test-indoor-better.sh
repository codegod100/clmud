#!/bin/bash
# Test that cars can't enter indoor areas

echo "=== Testing Indoor Restriction ==="
echo ""
echo "Test 1: Try to drive north to tavern (should fail)"
echo "Test 2: Exit car and walk north (should succeed)"
echo "Test 3: Try to drive south to garden (should fail)"
echo ""

(
  sleep 0.5
  echo "Tester"
  sleep 0.5
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "n"
  sleep 1
  echo "exit"
  sleep 0.5
  echo "n"
  sleep 1
  echo "s"
  sleep 0.5
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "s"
  sleep 1
  echo "quit"
) | nc localhost 4000

echo ""
echo "Expected results:"
echo "- Car should NOT be able to go north to tavern"
echo "- On foot, player SHOULD be able to go north to tavern"
echo "- Car should NOT be able to go south to garden"
