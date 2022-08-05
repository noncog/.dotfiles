#!/usr/bin/bash

dir="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -location 2 -yoffset 57 -no-fixed-num-lines -theme $dir/configs/cogmenu.rasi"

# Options
screenshot=" Screenshot"
brightness=" Brightness"
configuration=" Configs"
backups=" Backups"
controls=" Controls"
power=" Power"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/cogmenu/configs/error.rasi" -e "$1"
}

# Variable passed to rofi
options="$screenshot\n$brightness\n$configuration\n$backups\n$controls\n$power"

chosen="$(echo -e "$options" | $rofi_command -p 'menu' -dmenu -selected-row 1)"
case $chosen in
    $screenshot)
	if [[ -f "$dir/screenshot.sh" ]]; then
	    bash "$dir/screenshot.sh"
	else
	    msg "$screenshot file not found"
	fi
	;;
    $brightness)
	if [[ -f "$dir/brightness.sh" ]]; then
	    bash "$dir/brightness.sh" -selected-row 1
	else
	    msg "@brightness file not found"
	fi
        ;;
    $configuration)
	if [[ -f "$dir/configuration.sh" ]]; then
	    bash "$dir/configuration.sh"
	else
	    msg "@configuration file not found"
	fi
        ;;
    $backups)
	if [[ -f "$dir/rofi-borg/rofi-borg.sh" ]]; then
	    bash "$dir/rofi-borg/rofi-borg.sh"
	else
	    msg "@backups file not found"
	fi
        ;;
    $controls)
	if [[ -f "$dir/controls.sh" ]]; then
	    bash "$dir/controls.sh"
	else
	    msg "@settings file not found"
	fi
        ;;
    $power)
	if [[ -f "$dir/power.sh" ]]; then
	    bash "$dir/power.sh"
	else
	    msg "@power file not found"
	fi
        ;;
esac
