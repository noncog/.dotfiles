#!/usr/bin/env bash
# shellcheck disable=SC1091

update() {
    source "$CONFIG_DIR/colors.sh"
    COLOR="$BACKGROUND_2"
    if [ "$SELECTED" = "true" ]; then
        COLOR="$GREY"
    fi
    sketchybar --set "$NAME" icon.highlight="$SELECTED" \
        label.highlight="$SELECTED" \
        background.border_color="$COLOR"
}

mouse_clicked() {
    if [ "$MODIFIER" = "ctrl" ]; then
        yabai -m space --destroy "$SID"
        sketchybar --trigger windows_on_spaces
        sketchybar --trigger space_change
    else
        yabai -m space --focus "$SID" 2> /dev/null
    fi
}

case "$SENDER" in
    "mouse.clicked")
        mouse_clicked
        ;;
    *)
        update
        ;;
esac
