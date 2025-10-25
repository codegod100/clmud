#!/bin/bash
pkill -9 -f "sbcl.*mud.lisp"
sleep 2
sbcl --script mud.lisp > /tmp/mud.log 2>&1 &
PID=$!
sleep 4

cat << 'INPUT' | nc localhost 4000
driver
look
enter car
status
move east
move east  
ram goblin
ram goblin
quit
INPUT

sleep 1
kill -9 $PID 2>/dev/null
wait $PID 2>/dev/null
