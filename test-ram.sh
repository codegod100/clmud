#!/bin/bash
# Test script for the ram command

echo "Testing ram command in MUD..."
echo ""
echo "Commands to test:"
echo "1. go e (to riverbank where skiff is)"
echo "2. get skiff"
echo "3. enter skiff"
echo "4. go downstream (to river junction with goblin)"
echo "5. ram goblin (should work now!)"
echo ""
echo "Connecting to localhost:4000..."
echo ""

(
  sleep 0.5
  echo "TestPlayer"
  sleep 0.5
  echo "go e"
  sleep 0.5
  echo "get skiff"
  sleep 0.5
  echo "enter skiff"
  sleep 0.5
  echo "go downstream"
  sleep 0.5
  echo "ram goblin"
  sleep 1
  echo "quit"
) | nc localhost 4000

echo ""
echo "Test complete!"
