#!/bin/bash
# Test script for the quest system

PORT=4001

echo "Starting MUD server on port $PORT..."
sbcl --script mud.lisp --port $PORT &
SERVER_PID=$!

sleep 2

echo "Testing quest system..."
(
  sleep 0.5
  echo "TestPlayer"
  sleep 0.5
  echo "status"
  sleep 0.5
  echo "quest"
  sleep 0.5
  echo "quest start apple"
  sleep 0.5
  echo "quest"
  sleep 0.5
  echo "s"
  sleep 0.5
  echo "l"
  sleep 0.5
  echo "get apple"
  sleep 0.5
  echo "status"
  sleep 0.5
  echo "inv"
  sleep 0.5
  echo "quit"
) | nc localhost $PORT

kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null

echo "Test complete!"
