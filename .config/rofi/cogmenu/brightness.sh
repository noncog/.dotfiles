#!/usr/bin/bash

dir="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 47 -theme $dir/configs/cogmenu.rasi"

# Options
up=" Up"
down=" Down"

# Error msg
err_msg() {
    rofi -theme "$HOME/.config/rofi/cogmenu/configs/error.rasi" -e "$1"
}

# Variable passed to rofi
options="$up\n$down"

chosen="$(echo -e "$options" | $rofi_command -p 'brightness' -dmenu $1 $2)"
case $chosen in
    $up)
	if [[ -f "$dir/brightness_change.sh" ]]; then
	    bash "$dir/brightness_change.sh" + && bash "$dir/brightness.sh" -selected-row 0
	else
	    err_msg "$down failed."
	fi
	;;
    $down)
	if [[ -f "$dir/brightness_change.sh" ]]; then
	    bash "$dir/brightness_change.sh" - && bash "$dir/brightness.sh" -selected-row 1
	else
	    err_msg "$up failed."
	fi
        ;;
esac
