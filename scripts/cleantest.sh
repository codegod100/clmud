#!/bin/bash
PORT=7004
sbcl --script mud.lisp --port $PORT 2>/dev/null &
sleep 2
(echo "Tester"; sleep 0.5; echo "look"; sleep 0.5; echo "quit") | nc localhost $PORT
pkill -f "mud.lisp"
