#!/bin/bash
#
# This script initializes my X session as a systemd target remaining compatible
# with the default startx script used by Debian. This allows the use of systemd
# targets to orchestrate the building of a desktop environment. Eventually,
# startx/xinit should be replaced by my own version.

# This file is executed by startx and must remain executable, otherwise it will
# break. There are other concerns such as the symlink breaking or the shell
# interpreting the file as well. These concerns and others are all part of the
# motivation to build a better startx system. Debian even recommends it.

# For more information about using systemd to orchestrate an x session, see
# my target units in .dotfiles/home/.config/systemd/user.

# NOTE: xmodmap should also be replaced with a more robust system that cannot
#       lose its state when recovering from sleeping or the screen saver.
#       - These settings should also be made to be configurable.
# NOTE: The shebang is also hard-coded currently.
export XDG_SESSION_TYPE="x11"

xmodmap -e "clear lock"
xmodmap -e "clear control"
xmodmap -e "keycode 66 = Control_L"
xmodmap -e "add control = Control_L"
xset r rate 300 25

if [ "$HOSTNAME" = "Callisto" ]; then
    xrandr --auto --dpi 108 --output DP-0 --primary --mode 2560x1440 --rate 165.08 --output HDMI-0 --off
elif [ "$HOSTNAME" = "Europa" ]; then
    xrandr --auto --dpi 96
fi

exec systemctl --user start --wait xsession.target
