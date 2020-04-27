#!/usr/bin/env bash

# Set a background color

SETROOT=""
if which hsetroot >/dev/null; then
	SETROOT=hsetroot
elif which esetroot >/dev/null; then
	SETROOT=esetroot
elif which xsetroot >/dev/null; then
	SETROOT=xsetroot
fi
test -z $SETROOT || $SETROOT -solid "#203040"

OPENBOX_CONFIG_DIR=$(dirname $0)
if [ -f $OPENBOX_CONFIG_DIR/autostart-custom.sh ]; then
	$OPENBOX_CONFIG_DIR/autostart-custom.sh
fi

conky&
guake&
x-www-browser&
cairo-dock -o &
xmodmap ~/.Xmodmap
xrdb ~/.Xdefaults
