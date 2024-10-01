#!/bin/bash
# make display available
xrandr --auto
xrandr --dpi 108
xrandr --output DP-0 --primary --mode 2560x1440 --rate 165.08
xrandr --output HDMI-0 --off
# set backgrounds
nitrogen --restore
