#!/bin/bash
PORT=5008
sbcl --script mud.lisp --port $PORT 2>&1 | grep -v "^;" &
sleep 3

(
  echo "Hero"
  sleep 1
  echo "nw"
  sleep 1
  echo "quit"
) | nc localhost $PORT
pkill -f "mud.lisp"
