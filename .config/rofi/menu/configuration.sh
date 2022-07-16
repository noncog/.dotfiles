#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/configuration.rasi"

# Options
dunst="Dunst"
i3="i3"
polybar="Polybar"
picom="Picom"
kitty="Kitty"
emacs="Emacs"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/menu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$dunst\n$i3\n$polybar\n$picom\n$kitty\n$emacs"

chosen="$(echo -e "$options" | $rofi_command -p 'configs' -dmenu $1 $2)"
case $chosen in
    $dunst)
		if [[ -f "$HOME/.config/dunst/dunstrc" ]]; then
			emacsclient -a emacs $HOME/.config/dunst/dunstrc &
		else
			msg "$dunst config not found."
		fi
        ;;
    $i3)
		if [[ -f "$HOME/.config/i3/README.org" ]]; then
			emacsclient -a emacs $HOME/.config/i3/README.org &
		else
			msg "$i3 config not found."
		fi
        ;;
    $polybar)
		if [[ -f "$HOME/.config/polybar/config" ]]; then
			emacsclient -a emacs $HOME/.config/polybar/config &
		else
			msg "$polybar config not found."
		fi
        ;;
    $picom)
		if [[ -f "$HOME/.config/picom/picom.conf" ]]; then
			emacsclient -a emacs $HOME/.config/picom/picom.conf &
		else
			msg "$picom config not found."
		fi
        ;;
    $kitty)
		if [[ -f "$HOME/.config/kitty/kitty.conf" ]]; then
			emacsclient -a emacs $HOME/.config/kitty/kitty.conf &
		else
			msg "$kitty config not found."
		fi
        ;;
    $emacs)
		if [[ -f "$HOME/.emacs.d/README.org" ]]; then
			emacsclient -a emacs $HOME/.emacs.d/README.org &
		else
			msg "$kitty config not found."
		fi
        ;;
esac
