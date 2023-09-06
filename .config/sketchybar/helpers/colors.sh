#!/usr/bin/env bash
# shellcheck disable=SC2034

BLACK=0xff000000
WHITE=0xffffffff
TRANSPARENT=0x00000000

if [[ "$THEME" -eq 1 ]]; then
    # Define Dracula colors
    BACKGROUND=0xff282a36
    FOREGROUND=0xfff8f8f2
    FOCUSED=0xff44475a
    BLUE=0xff6272a4
    CYAN=0xff8be9fd
    GREEN=0xff50fa7b
    ORANGE=0xffffb86c
    PINK=0xffff79c6
    PURPLE=0xffbd93f9
    RED=0xffff5555
    YELLOW=0xfff1fa8c
    # Translate colors to SketchyBar
    BAR_COLOR=$BACKGROUND
    BORDER_COLOR=$FOCUSED
    ICON_COLOR=$FOREGROUND
    LABEL_COLOR=$FOREGROUND
    POPUP_COLOR=$BACKGROUND
    POPUP_BORDER_COLOR=$FOCUSED
    #HIGHLIGHT_COLOR=
elif [[ "$THEME" -eq 2 ]]; then
    # Define Catppuccin Frappe colors
    ROSEWATER=0xfff2d5cf
    FLAMINGO=0xffeebebe
    PINK=0xfff4b8e4
    MAUVE=0xffca9ee6
    RED=0xffe78284
    MAROON=0xffea999c
    PEACH=0xffef9f76
    YELLOW=0xffe5c890
    GREEN=0xffa6d189
    TEAL=0xff81c8be
    SKY=0xff99d1db
    SAPPHIRE=0xff85c1dc
    BLUE=0xff8caaee
    LAVENDER=0xffbabbf1
    TEXT=0xffc6d0f5
    SUBTEXT1=0xffb5bfe2
    SUBTEXT0=0xffa5adce
    OVERLAY2=0xff949cbb
    OVERLAY1=0xff838ba7
    OVERLAY0=0xff737994
    SURFACE2=0xff626880
    SURFACE1=0xff51576d
    SURFACE0=0xff414559
    BASE=0xff303446
    MANTLE=0xff292c3c
    CRUST=0xff232634
    # Translate colors to SketchyBar
    BAR_COLOR=$BASE
    BORDER_COLOR=$SURFACE1
    ICON_COLOR=$TEXT
    LABEL_COLOR=$TEXT
    POPUP_COLOR=$BASE
    POPUP_BORDER_COLOR=$TEXT
    #HIGHLIGHT_COLOR=
else
    # Define Catppuccin Mocha colors
    ROSEWATER=0xfff5e0dc
    FLAMINGO=0xfff2cdcd
    PINK=0xfff5c2e7
    MAUVE=0xffcba6f7
    RED=0xfff38ba8
    MAROON=0xffeba0ac
    PEACH=0xfffab387
    YELLOW=0xfff9e2af
    GREEN=0xffa6e3a1
    TEAL=0xff94e2d5
    SKY=0xff89dceb
    SAPPHIRE=0xff74c7ec
    BLUE=0xff89b4fa
    LAVENDER=0xffb4befe
    TEXT=0xffcdd6f4
    SUBTEXT1=0xffbac2de
    SUBTEXT0=0xffa6adc8
    OVERLAY2=0xff9399b2
    OVERLAY1=0xff7f849c
    OVERLAY0=0xff6c7086
    SURFACE2=0xff585b70
    SURFACE1=0xff45475a
    SURFACE0=0xff313244
    BASE=0xff1e1e2e
    MANTLE=0xff181825
    CRUST=0xff11111b
    TRANSPARENT=0x00000000
    # Translate colors to SketchyBar
    BAR_COLOR=$BASE
    BORDER_COLOR=$SURFACE1
    ICON_COLOR=$TEXT
    LABEL_COLOR=$TEXT
    POPUP_COLOR=$BASE
    POPUP_BORDER_COLOR=$TEXT
    #HIGHLIGHT_COLOR=
fi
