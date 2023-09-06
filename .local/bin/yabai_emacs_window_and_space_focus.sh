#!/usr/bin/env bash

if [[ "$1" =~ ^(east|west)$ ]]; then
  yabai -m window --focus "$1" || yabai -m space --focus "next"
else
  yabai -m window --focus "$1"
fi
