#!/usr/bin/bash

directory="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 57 -theme $directory/configs/cogmenu.rasi"

# options
screen=" Screen"
area=" Area"
window=" Window"

# error msg
err_msg() {
    rofi -theme "$directory/configs/error.rasi" -e "$1"
}

# variable passed to rofi
options="$screen\n$area\n$window"

selection="$(echo -e "$options" | $rofi_command -p 'scrot' -dmenu)"
case $selection in
    $screen)
	if [[ -f /usr/bin/scrot ]]; then
	    sleep 1; scrot -d 1 $HOME/Pictures/screenshots/screen-%b%d::%H%M%S.png
	else
	    err_msg "Scrot -d failed."
	fi
	;;
    $area)
	if [[ -f /usr/bin/scrot ]]; then
	    scrot -s $HOME/Pictures/screenshots/area-%b%d::%H%M%S.png
	else
	    err_msg "Scrot -s failed."
	fi
	;;
    $window)
	if [[ -f /usr/bin/scrot ]]; then
	    sleep 1; scrot -u $HOME/Pictures/screenshots/window-%b%d::%H%M%S.png
	else
	    err_msg "Scrot -u failed."
	fi
	;;
esac
