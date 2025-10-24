#!/bin/bash
PORT=4005

sbcl --script mud.lisp --port $PORT 2>&1 | grep -E "(DEBUG.*quest|Quest)" &
SERVER_PID=$!
sleep 2

(
  echo "QuestTest"
  sleep 0.5
  echo "quest start apple"
  sleep 1
  echo "quit"
) | nc localhost $PORT 2>&1 | grep -i "quest"

kill %1 2>/dev/null
wait
