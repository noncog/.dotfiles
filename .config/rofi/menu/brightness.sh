#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/brightness.rasi"

# Options
up=" Up"
down=" Down"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/menu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$up\n$down"

chosen="$(echo -e "$options" | $rofi_command -p 'brightness' -dmenu $1 $2)"
case $chosen in
    $up)
		if [[ -f "$dir/brightness_change.sh" ]]; then
			bash "$dir/brightness_change.sh" + && bash "$dir/brightness.sh" -selected-row 0
		else
			msg "$down failed."
		fi
        ;;
    $down)
		if [[ -f "$dir/brightness_change.sh" ]]; then
			bash "$dir/brightness_change.sh" - && bash "$dir/brightness.sh" -selected-row 1
		else
			msg "$up failed."
		fi
        ;;
esac
