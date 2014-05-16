#!/bin/bash

if [ ! -d ~/.themes/Bluebird ]; then
	echo "Install Bluebird theme..."
	git clone git@github.com:shimmerproject/Bluebird.git ~/.themes/Bluebird
else
	echo "Already installed!!!"
fi
