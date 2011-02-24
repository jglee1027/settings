#!/bin/bash
settings_path=$(dirname $0)

diff_file=/tmp/install.diff
diff_cp() {
	if [ -z "$1" -o -z "$2" ]; then
		return 0
	fi
	
	if [ ! -e $2 ]; then
		cp $1 $2
	fi
	
	diff -u $2 $1 > $diff_file
	local diff_return=$?
	if [ $diff_return -eq 0 ]; then
		return 1
	fi

	while [ 1 ]; do
		echo cp: overwrite "'$2' [Y/n/d(diff)]? "
		read choice
		if [ "$choice" = "Y" -o "$choice" = "y" ]; then
			cp $1 $2
			return 2
		elif [ "$choice" = "N" -o "$choice" = "n" ]; then
			return 3
		elif [ "$choice" = "d" ]; then
			more $diff_file
		fi
	done
}

echo emacs setting...
diff_cp $settings_path/emacs/.emacs ~/.emacs
diff_cp $settings_path/emacs/linux/.Xresources ~/.Xresources
echo emacs setting... OK

echo vim setting...
diff_cp $settings_path/vim/.vimrc ~/.vimrc
echo vim setting... OK

echo font setting...
diff_cp $settings_path/fonts/.fonts.conf ~/
echo font setting... OK

echo git setting...
diff_cp $settings_path/.gitconfig ~/.gitconfig
echo git setting... OK

echo gdb setting...
diff_cp $settings_path/.gdbinit ~/.gdbinit
echo gdb setting... OK

if [ -d ~/.moc ]; then
	echo moc setting...
	diff_cp $settings_path/.moc/config ~/.moc/config
	echo moc setting... OK
fi

if [ -d ~/.mplayer ]; then
	echo mplayer setting...
	diff_cp $settings_path/.mplayer/config ~/.mplayer/config
	echo mplayer setting... OK
fi

echo conkeror setting...
diff_cp $settings_path/.conkerorrc ~/.conkerorrc

# make conkeror bookmarks synchronize with firefox bookmarks
firefox_config_path=~/.mozilla/firefox
conkeror_config_path=~/.conkeror.mozdev.org/conkeror
if [ -d $firefox_config_path -a -d $conkeror_config_path ]; then
	firefox_bookmarks=$firefox_config_path/$(cat $firefox_config_path/profiles.ini | grep Path | awk 'BEGIN { FS = "=" } { print $2 }')
	conkeror_bookmarks=$conkeror_config_path/$(cat $conkeror_config_path/profiles.ini | grep Path | awk 'BEGIN { FS = "=" } { print $2 }')
	cp $firefox_bookmarks/places.sqlite $conkeror_bookmarks/places.sqlite
fi
echo conkeror setting...OK
