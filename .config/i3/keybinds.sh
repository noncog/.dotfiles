#!/bin/bash
kitty -- bash -c "grep -e '^[^#]*bind' ~/.config/i3/config; exec bash"
