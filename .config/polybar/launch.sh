#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use 
# polybar-msg cmd quit
# Otherwise you can use the nuclear option:
killall -q polybar

# Launch default bar and/or others
echo "---" | tee -a /tmp/polybar_default_bar.log
polybar default_bar 2>&1 | tee -a /tmp/polybar_default_bar.log & disown

echo "Bars launched..."
