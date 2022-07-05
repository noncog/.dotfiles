#!/bin/bash
# make display available
xrandr --auto
# rotate laptop display
xrandr --output eDP-1-1 --rotate right --auto
# set monitor positions
xrandr --output eDP-1-1 --left-of HDMI-0
# set primary display
xrandr --output HDMI-0 --primary
# set refresh rate of primary
xrandr --output HDMI-0 --mode 2560x1440 --rate 143.91
# set backgrounds
nitrogen --restore
