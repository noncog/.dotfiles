#!/usr/bin/env sh

yabai -m query --windows --space | jq -e --arg app "$1" '.[] | select(.app == $app) | .["has-focus"]' >/dev/null

if [ "$?" -eq 0 ]; then
    osascript -e 'tell application "System Events" to set visible of process "'"$1"'" to false'
    yabai -m window --focus mouse
else
    open -a "$1"
fi
