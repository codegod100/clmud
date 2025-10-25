#!/bin/bash
# Full test of ram command - get car, enter it, then ram goblin

echo "=== FULL RAM COMMAND TEST ==="
echo "This will:"
echo "1. Get the car from Village Square"
echo "2. Enter the car"
echo "3. Go to Whispering Wood"
echo "4. Ram the goblin!"
echo ""

(
  sleep 0.5
  echo "RamTester"
  sleep 0.5
  echo "get car"
  sleep 0.5
  echo "enter car"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "e"
  sleep 0.5
  echo "look"
  sleep 0.5
  echo "ram goblin"
  sleep 1
  echo "ram goblin"
  sleep 1
  echo "ram goblin"
  sleep 1
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost 4000
