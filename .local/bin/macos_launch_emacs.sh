#!/usr/bin/env sh

# This script enables one keybind to launch the daemon/client for Emacs
# if it doesn't exist or to focus an existing frame if it does exist.

# Get the default Chemacs2 profile
read -r default_profile <"$HOME/.config/chemacs/profile"

# Check if the server for that profile is running and if it is check if it has a frame.
frame_check=$(emacsclient -s $default_profile -e "(> (length (frame-list)) 1)")

# if it is running and we have frame, then focus it.
if [ $? ] && [ "$frame_check" = "t" ]; then
    osascript -e "tell application \"Emacs\" to activate"
    exit 0
else
    # if not running or have no frame, then create a frame/daemon.
    emacsclient -n -c -s $default_profile -a=''
fi
