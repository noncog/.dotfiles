#!/usr/bin/env sh

printf "%s" "$(yabai -m query --spaces | jq ".[] | select(.id==$1).index")" > "$HOME"/.config/yabai/state/focused_space
