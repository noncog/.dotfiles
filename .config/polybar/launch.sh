#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use 
# polybar-msg cmd quit
# Otherwise you can use the nuclear option:
killall -q polybar

# Launch default bar and/or others
echo "---" | tee -a /tmp/polybar_default_bar.log /tmp/polybar_clear.log /tmp/polybar_left.log /tmp/polybar_center.log /tmp/polybar_right.log
polybar default_bar 2>&1 | tee -a /tmp/polybar_default_bar.log & disown
#polybar clear 2>&1 | tee -a /tmp/polybar_transparent.log & disown
#polybar left 2>&1 | tee -a /tmp/polybar_left.log & disown
#polybar center 2>&1 | tee -a /tmp/polybar_center.log & disown
#polybar right 2>&1 | tee -a /tmp/polybar_right.log & disown
echo "Bars launched..."
