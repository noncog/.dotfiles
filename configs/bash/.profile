# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Set PATH, MANPATH, etc., for Homebrew.
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Set XDG directories
export XDG_CONFIG_HOME="$HOME/.config"

# Set Emacs as system editor.
export EMACSDIR="$HOME/.config/emacs"
export DOOMDIR="$HOME/.config/doom"

export EDITOR="emacsclient -c -a=''"
export VISUAL="$EDITOR"

# Set terminal.
export TERMINAL=/usr/bin/kitty

# Add Jetbrains Toolbox to $PATH.
export PATH="$PATH:/home/jake/.local/share/JetBrains/Toolbox/scripts"

# Add Clangd/Llvm for LSP mode.
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

# Add GNU grep to system instead of BSD grep.
PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
