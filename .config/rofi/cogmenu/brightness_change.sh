#!/usr/bin/bash

# options
monitor="eDP-1-1" # discover using: xrandr | grep " connected"
step_percent="10" # percent to step by...

# brightness values
current_brightness=$( xrandr --verbose --current | grep ^"$monitor" -A5 | tail -n1 )

# floating brightness values
current_brightness="${current_brightness##* }"              # brightness level in float format
left_floating_brightness=${current_brightness%%"."*}        # extract left of decimal point
right_floating_brightness=${current_brightness#*"."}        # extract right of decimal point

# initialize new brightness
new_brightness="0"

# increase
if [ "$1" == "+" ]; then
    # set dunst notification title
    dunst_title="Brightness increased:"
    
    # step the brightness
    new_brightness=$((right_floating_brightness + step_percent))

    # check if already 1.0
    if [ $left_floating_brightness != "0" ]; then
        # leave same if already 1.0
        new_brightness=$current_brightness

    # check if greater than 100 or 1.0 in xrandr terms
    elif [[ $new_brightness -ge "100" ]]; then
        # set to 100
        new_brightness="100"

        # convert to decimal 1.0
        new_brightness="${new_brightness:0:1}.${new_brightness:1:1}"
    else
        # convert to decimal 0.x
        new_brightness="${left_floating_brightness}.${new_brightness:0:2}"
    fi
# decrease
elif [ "$1" == "-" ]; then
    # set dunst notification title
    dunst_title="Brightness decreased:"

    # check if 100, it breaks this method of incrementing
    if [ "$right_floating_brightness" == "0" ]; then
        left_floating_brightness="0"
        right_floating_brightness="100"
    fi

    # step the brightness
    new_brightness=$((right_floating_brightness - step_percent))

    # check if already 0.1 or 10 in xrandr terms
    if [ "$right_floating_brightness" == "10" ]; then
        # leave same if already 0.1 or 10
        new_brightness=$current_brightness

    # check if lower than 10 or 0.1 in xrandr terms
    elif [[ $new_brightness -le "10" ]]; then
        # set to 10
        new_brightness="0.1"
    else
        # convert to decimal 0.x
        new_brightness="${left_floating_brightness}.${new_brightness:0:2}"
    fi
else
    dunstify "No valid brightness change argument: +/-"
fi

# change brightness
xrandr --output "$monitor" --brightness "$new_brightness"

# notify results
dunstify "$dunst_title" "Now: $new_brightness"
