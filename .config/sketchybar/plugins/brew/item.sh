#!/usr/bin/env bash
# shellcheck disable=SC1091

# Trigger the brew_udpate event when brew update or upgrade is run from cmdline
# e.g. via function in .zshrc
brew=(
    icon=􀐛
    label=?
    padding_right=4
    script="$CONFIG_DIR/plugins/brew/plugin.sh"
)

sketchybar --add event brew_update \
    --add item brew e \
    --set brew "${brew[@]}" \
    --subscribe brew brew_update
