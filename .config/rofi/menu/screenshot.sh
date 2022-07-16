#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/screenshot.rasi"

# Options
screen=" Screen"
area=" Area"
window=" Window"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/menu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$screen\n$area\n$window"

chosen="$(echo -e "$options" | $rofi_command -p 'scrot' -dmenu -selected-row 1)"
case $chosen in
    $screen)
		if [[ -f /usr/bin/scrot ]]; then
			sleep 1; scrot -d 1 $HOME/.screenshots/screen-%b%d::%H%M%S.png
		else
			msg "Scrot -d failed."
		fi
        ;;
    $area)
		if [[ -f /usr/bin/scrot ]]; then
			scrot -s $HOME/.screenshots/area-%b%d::%H%M%S.png
		else
			msg "Scrot -s failed."
		fi
        ;;
    $window)
		if [[ -f /usr/bin/scrot ]]; then
		    sleep 1; scrot -u $HOME/.screenshots/window-%b%d::%H%M%S.png
		else
			msg "Scrot -u failed."
		fi
        ;;
esac
