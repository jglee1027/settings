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

tint2&
conky&
tilda&
x-www-browser&
