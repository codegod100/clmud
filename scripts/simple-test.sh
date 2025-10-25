#!/bin/bash
PORT=4010
sbcl --script mud.lisp --port $PORT >/dev/null 2>&1 &
sleep 2
(
  echo "Hero"
  sleep 0.3
  echo "status"
  sleep 0.3
  echo "quest start apple"
  sleep 0.3
  echo "s"
  sleep 0.3
  echo "get apple"
  sleep 0.3
  echo "status"
  sleep 0.3
  echo "quit"
) | nc localhost $PORT 2>&1 | grep -E "^(Hero|Health|XP|Quest|You (get|gained)|LEVEL|elder)"
pkill -f "mud.lisp --port $PORT"
