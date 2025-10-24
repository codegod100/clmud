#!/bin/bash
PORT=4008

sbcl --script mud.lisp --port $PORT 2>&1 | grep -v "^DEBUG:" &
SERVER_PID=$!
sleep 2

echo "=== Testing Quest System ==="
(
  sleep 0.5
  echo "Hero"
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
  echo "look"
  sleep 0.5
  echo "get apple"
  sleep 0.5
  echo "status"
  sleep 0.5
  echo "inv"
  sleep 0.5
  echo "quit"
) | nc localhost $PORT 2>&1 | grep -v "^DEBUG:" | grep -E "(Hero|Quest|Level|XP|Health|Mana|Elder|apple|gets)"

kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null

echo "=== Test Complete ==="
