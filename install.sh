#!/bin/sh

~/settings/emacs/install.sh
~/settings/vim/install.sh
~/settings/fonts/install.sh

cp ~/settings/.gitconfig ~/
cp ~/settings/.conkerorrc ~/

if [ -d ~/.moc ]; then
	cp ~/settings/.moc/config ~/.moc/
fi
