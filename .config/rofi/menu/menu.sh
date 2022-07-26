#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/menu.rasi"

# Options
screenshot=" Screenshot"
brightness=" Brightness"
configuration=" Configs"
backups=" Backups"
settings=" Settings"
power=" Power"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/menu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$screenshot\n$brightness\n$configuration\n$backups\n$settings\n$power"

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
	    bash "$dir/brightness.sh"
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
    $settings)
	if [[ -f "$dir/settings.sh" ]]; then
	    bash "$dir/settings.sh"
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
