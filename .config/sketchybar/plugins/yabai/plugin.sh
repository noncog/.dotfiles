#!/bin/bash
# NOTE: Floating apps on line 14 are ignor
windows_on_spaces() {
  CURRENT_SPACES="$(yabai -m query --displays | jq -r '.[].spaces | @sh')"

  args=(--set spaces_bracket drawing=off
    --set '/space\..*/' background.drawing=on
    --animate sin 10)

  while read -r line; do
    for space in $line; do
      icon_strip=" "
      apps=$(yabai -m query --windows --space $space | jq -r ".[].app")
      if [ "$apps" != "" ]; then
        while IFS= read -r app; do
          if ! [[ "$app" =~ (kitty|Discord|Spotify) ]]; then
            icon_strip+=" $($CONFIG_DIR/plugins/yabai/icon_map.sh "$app")"
          fi
        done <<<"$apps"
      fi
      args+=(--set space.$space label="$icon_strip" label.drawing=on)
    done
  done <<<"$CURRENT_SPACES"

  sketchybar -m "${args[@]}"
}

mouse_clicked() {
  yabai -m window --toggle float
  window_state
}

case "$SENDER" in
  "mouse.clicked")
    mouse_clicked
    ;;
  "forced")
    exit 0
    ;;
  "windows_on_spaces" | "space_change")
    windows_on_spaces
    ;;
esac
