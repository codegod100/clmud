#!/bin/bash
PORT=5002
sbcl --script mud.lisp --port $PORT >/dev/null 2>&1 &
sleep 2

(
  echo "Warrior"
  sleep 0.3
  echo "e"
  sleep 0.3
  echo "look"
  sleep 0.3
  echo "look bandit"
  sleep 0.3
  echo "attack bandit"
  sleep 0.3
  echo "attack bandit"
  sleep 0.3
  echo "attack bandit"
  sleep 0.3
  echo "attack bandit"
  sleep 0.5
  echo "look"
  sleep 0.3
  echo "get steel-sword"
  sleep 0.3
  echo "equip steel-sword"
  sleep 0.3
  echo "status"
  sleep 0.3
  echo "quit"
) | nc localhost $PORT 2>&1 | grep -vE "DEBUG:|ROOM|EXITS"
pkill -f "mud.lisp --port $PORT"
