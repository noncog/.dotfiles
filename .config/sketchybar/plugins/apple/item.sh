#!/bin/bash

# Icons
APPLE=􀣺

# Scripts.
POPUP_CLICK_SCRIPT='sketchybar --set $NAME popup.drawing=toggle'
POPUP_OFF='sketchybar --set apple.logo popup.drawing=off'

# Setup the Apple item.
apple_logo=(
  icon=$APPLE
  icon.font="$FONT:Black:16.0"
  icon.color=$GREEN
  padding_right=15
  label.drawing=off
  click_script="$POPUP_CLICK_SCRIPT"
  popup.height=35
)

# Setup each menu item.
apple_prefs=(
  label="Preferences"
  click_script="open -a 'System Preferences'; $POPUP_OFF"
)
apple_activity=(
  label="Activity Monitor"
  click_script="open -a 'Activity Monitor'; $POPUP_OFF"
)
apple_sleep=(
  label="Sleep"
  click_script="pmset displaysleepnow; $POPUP_OFF"
)
apple_shutdown=(
  label="Shutdown"
  click_script="shutdown -h now; $POPUP_OFF"
)
apple_restart=(
  label="Restart"
  click_script="shutdown -r now; $POPUP_OFF"
)

# Add each item and their settings.
sketchybar --add item apple.logo left                  \
           --set apple.logo "${apple_logo[@]}"         \
                                                       \
           --add item apple.prefs popup.apple.logo     \
           --set apple.prefs "${apple_prefs[@]}"       \
                                                       \
           --add item apple.activity popup.apple.logo  \
           --set apple.activity "${apple_activity[@]}" \
                                                       \
           --add item apple.sleep popup.apple.logo     \
           --set apple.sleep "${apple_sleep[@]}"       \
                                                       \
           --add item apple.shutdown popup.apple.logo  \
           --set apple.shutdown "${apple_shutdown[@]}" \
                                                       \
           --add item apple.restart popup.apple.logo   \
           --set apple.restart "${apple_restart[@]}"
