#!/usr/bin/env bash
# shellcheck disable=SC1091

yabai=(
    icon.width=0
    label.width=0
    script="$CONFIG_DIR/plugins/yabai/plugin.sh"
    icon.font="$FONT:Bold:16.0"
    associated_display=active
)

sketchybar --add event window_focus \
    --add event windows_on_spaces \
    --add item yabai e \
    --set yabai "${yabai[@]}" \
    --subscribe yabai window_focus \
    space_change \
    windows_on_spaces \
    mouse.clicked
