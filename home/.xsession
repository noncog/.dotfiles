#!/bin/bash

# This file is executed by startx and must remain executable, otherwise it will
# break. There are other concerns such as the symlink breaking or the shell
# interpreting the file as well. These concerns and others are all part of the
# motivation to build a better startx system. Debian even recommends it.

# NOTE: xmodmap should also be replaced with a more robust system that cannot
#       lose its state when recovering from sleeping or the screen saver.
#       - These settings should also be made to be configurable.
# NOTE: The shebang is also hard-coded currently.

export XDG_SESSION_TYPE="x11"

# TODO: Fix losing this on keyboard disconnect.

xset r rate 300 25

if [ "$HOSTNAME" = "Callisto" ]; then
    xrandr --auto --dpi 109 --output DP-0 --primary --mode 2560x1440 --rate 165.08
elif [ "$HOSTNAME" = "Europa" ]; then
    xrandr --auto --dpi 96
fi

exec i3
