#!/usr/bin/env bash

# This script takes as input the desired app name recognized by Yabai
# then focuses the latest created window of that app name if one exists.
desired_window="$1"
newest_window=$(yabai -m query --windows | jq '[map(select(."app" == "'"$desired_window"'"))[].id] | max')

if [[ "$newest_window" != "null" ]]; then
    yabai -m window --focus "$newest_window"
fi
