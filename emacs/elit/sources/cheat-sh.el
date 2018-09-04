#!/bin/bash

require() {
	echo ""
}

description() {
	echo ""
}

target() {
	echo "$TARGET_DIR/cheat-sh.el"
}

install() {
	git clone https://github.com/davep/cheat-sh.el.git
}

remove() {
	rm -vrf $(target)
}

eval $1
