#!/usr/bin/bash

dir="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -theme $dir/configs/cogmenu.rasi"

# Options
dunst="Dunst"
i3="i3"
polybar="Polybar"
picom="Picom"
kitty="Kitty"
cogmacs="Emacs"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/cogmenu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$dunst\n$i3\n$polybar\n$picom\n$kitty\n$cogmacs"

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
    $cogmacs)
		if [[ -f "$HOME/.config/cogmacs/README.org" ]]; then
			emacsclient -a emacs $HOME/.emacs.d/README.org &
		else
			msg "$kitty config not found."
		fi
        ;;
esac
