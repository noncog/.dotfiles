#!/bin/bash
# This file is executed by startx and must remain executable.
# This system keeps the setup using the startx/initx program but
# runs my x session as a systemd target allowing me to depend
# on it using other systemd targets.
# Eventually this should be replaced by a custom version
# developed by myself.
export XDG_SESSION_TYPE="x11"

xmodmap -e "clear lock"
xmodmap -e "clear control"
xmodmap -e "keycode 66 = Control_L"
xmodmap -e "add control = Control_L"
xset r rate 300 25

if [ "$HOSTNAME" = "Callisto" ]; then
    xrandr --auto --dpi 108 --output DP-0 --primary --mode 2560x1440 --rate 165.08 --output HDMI-0 --off
elif  [ "$HOSTNAME" = "Europa" ]; then
    xrandr --auto --dpi 96
fi

exec systemctl --user start --wait xsession.target
