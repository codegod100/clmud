#!/bin/bash
PORT=4002

sbcl --script mud.lisp --port $PORT &
SERVER_PID=$!
sleep 2

(
  sleep 0.5
  echo "QuestTest"
  sleep 0.5
  echo "quest start apple"
  sleep 0.5
  echo "quest"
  sleep 0.5
  echo "s"
  sleep 0.5
  echo "get apple"
  sleep 1
  echo "quit"
) | nc localhost $PORT 2>&1 | grep -E "(Quest|apple|XP|Level)"

kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null
