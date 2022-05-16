#!/usr/bin/env perl
#
# brightness-backlight.pl
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
use File::Basename;

my $step = "10%";
my $value = "";

if (defined $ARGV[0]) {
    if ($ARGV[0] eq 'up') {
        $value = "+$step";
    } elsif ($ARGV[0] eq 'down') {
        $value = "$step-";
    }
} else {
    my $name = basename($0);
    print "\nUsage: $name <up|down>\n\n";
    exit 1;
}

my $brightness = `brightnessctl set $value | grep -i "current brightness"`;
chomp($brightness);
$brightness =~ s/^\s*//;

print "$brightness\n";
`notify-send "$brightness"`;
