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

echo -ne "emacs setting...\r"
diff_cp $settings_path/emacs/.emacs ~/.emacs

fc-list | grep NanumGothic_AndaleMono > /dev/null
if [ $? -eq 0 ]; then
	diff_cp $settings_path/emacs/linux/.Xresources ~/.Xresources
fi

emacs --batch -f batch-byte-compile ~/.emacs 2> ~/.emacs.elc.log
echo emacs setting... OK

echo -ne "vim setting...\r"
diff_cp $settings_path/vim/.vimrc ~/.vimrc
echo vim setting... OK

echo -ne "xmodmap setting...\r"
diff_cp $settings_path/.Xmodmap ~/.Xmodmap
echo xmodmap setting... OK

echo -ne "font setting...\r"
diff_cp $settings_path/fonts/.fonts.conf ~/.fonts.conf
echo font setting... OK

echo -ne "git setting...\r"
diff_cp $settings_path/.gitconfig ~/.gitconfig
echo git setting... OK

echo -ne "gdb setting...\r"
diff_cp $settings_path/.gdbinit ~/.gdbinit
echo gdb setting... OK

if [ -d ~/.moc ]; then
	echo -ne "moc setting...\r"
	diff_cp $settings_path/.moc/config ~/.moc/config
	echo moc setting... OK
fi

if [ -d ~/.mplayer ]; then
	echo -ne "mplayer setting...\r"
	diff_cp $settings_path/.mplayer/config ~/.mplayer/config
	echo mplayer setting... OK
fi

echo -ne "conkeror setting...\r"
diff_cp $settings_path/.conkerorrc ~/.conkerorrc

if [ -d ~/.config/terminator ]; then
	echo -ne "terminator setting...\r"
	diff_cp $settings_path/config/terminator/config ~/.config/terminator/config
	echo terminator setting... OK
fi

# make conkeror bookmarks synchronize with firefox bookmarks
firefox_config_path=~/.mozilla/firefox
conkeror_config_path=~/.conkeror.mozdev.org/conkeror
if [ -d $firefox_config_path -a -d $conkeror_config_path ]; then
	firefox_bookmarks=$firefox_config_path/$(cat $firefox_config_path/profiles.ini | grep Path | awk 'BEGIN { FS = "=" } { print $2 }')
	conkeror_bookmarks=$conkeror_config_path/$(cat $conkeror_config_path/profiles.ini | grep Path | awk 'BEGIN { FS = "=" } { print $2 }')
	cp $firefox_bookmarks/places.sqlite $conkeror_bookmarks/places.sqlite
fi
echo conkeror setting... OK
