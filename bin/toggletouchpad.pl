#!/usr/bin/env perl

# Toggle Touchpad
#
# Copyright (C) 2022  <jglee1027@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

use strict;
use warnings;
use utf8;

my $touchpad_id = `xinput list | grep -i Touchpad`;
$touchpad_id =~ s/.*id=(\d*).*$/$1/;
chomp($touchpad_id);

if ($touchpad_id eq "") {
    print "Error: Not found Touchpad\n";
    exit 1;
}

my @props = `xinput list-props $touchpad_id`;
my ($enabled) = grep(/Device Enabled/, @props);
$enabled =~ s/.*:\s*(\d)$/$1/;
chomp($enabled);

if ($enabled eq "1") {
    `xinput disable $touchpad_id`;
    `notify-send "Touchpad OFF"`;
} else {
    `xinput enable $touchpad_id`;
    `xinput set-prop $touchpad_id "libinput Natural Scrolling Enabled" 1`;
    `notify-send "Touchpad ON"`;
}
