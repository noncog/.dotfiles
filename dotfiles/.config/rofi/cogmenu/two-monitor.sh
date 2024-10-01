#!/usr/bin/env bash

#!/bin/bash
# make display available
xrandr --auto
xrandr --dpi 108
xrandr --output DP-0 --primary --mode 2560x1440 --rate 165.08
xrandr --output HDMI-0 --right-of DP-0 --mode 1920x1080 --rate 60.00
# set backgrounds
nitrogen --restore
