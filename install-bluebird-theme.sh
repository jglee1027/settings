#!/bin/bash

if [ ! -d ~/.themes/Bluebird ]; then
	echo "Install Bluebird theme..."
	git clone https://github.com/shimmerproject/Bluebird.git ~/.themes/Bluebird
else
	echo "Already installed!!!"
fi
