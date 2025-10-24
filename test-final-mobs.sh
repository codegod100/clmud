#!/bin/bash
PORT=7005
sbcl --script mud.lisp --port $PORT 2>/dev/null &
sleep 2
(
  echo "Warrior"
  sleep 0.5
  echo "nw"
  sleep 0.5
  echo "look"
  sleep 1
  echo "quit"
) | nc localhost $PORT | tail -20
pkill -f "mud.lisp"
