#!/bin/bash
pkill -9 -f "sbcl.*mud.lisp" 2>/dev/null
sleep 1
sbcl --script mud.lisp > /tmp/mud.log 2>&1 &
PID=$!
sleep 3

cat << 'INPUT' | nc localhost 4000
adventurer
inventory
move east
move east
attack goblin
attack goblin
attack goblin
attack goblin
get rusty-dagger
inventory
equip rusty-dagger
inventory
status
move west
move west
move northwest
cast fireball skeleton
cast fireball skeleton
get bone-sword
get rusted-chainmail
inventory
equip bone-sword
inventory
equip rusted-chainmail
inventory
status
unequip weapon
inventory
status
quit
INPUT

kill $PID 2>/dev/null
wait $PID 2>/dev/null
