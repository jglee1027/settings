#!/bin/bash

MONITORS=`xrandr -q | grep -e '\<connected\>' | wc -l`
ACTIVE_WIN_ID=`xprop -root | grep -e '^_NET_ACTIVE_WINDOW' | awk '{print $5}'`
ACTIVE_WIN_POS_X=`xwininfo -id $ACTIVE_WIN_ID  | grep 'Absolute upper-left X' | cut -f 2 -d ':'`

WIDTH_OF_SCREEN=`xdpyinfo | grep 'dimensions:' | cut -f 2 -d ':' | cut -f 1 -d 'x'`
HALF_OF_SCREEN=$(($WIDTH_OF_SCREEN / 2))
HALF_OF_MONITOR=$(($HALF_OF_SCREEN / $MONITORS))

# left in a monitor
if [ $ACTIVE_WIN_POS_X -ge $HALF_OF_SCREEN ]; then
	X=$HALF_OF_SCREEN
else
	X=0
fi
WIDTH=$HALF_OF_MONITOR

# left in the screen
if [ "$1" = "monitor" ]; then
	X=0
	WIDTH=$HALF_OF_SCREEN
fi

wmctrl -r :ACTIVE: -b add,maximized_vert && wmctrl -r :ACTIVE: -e 0,$X,0,$WIDTH,-1