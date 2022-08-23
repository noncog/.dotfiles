#!/bin/bash
# Terminate already running bar instances

# If all your bars have ipc enabled, you can use
# polybar-msg cmd quit
# Otherwise you can use the nuclear option:
killall -q polybar

# launch default bar
echo "---" | tee -a /tmp/polybar_default_bar.log
polybar default_bar 2>&1 | tee -a /tmp/polybar_default_bar.log & disown
echo "default bar launched..."
