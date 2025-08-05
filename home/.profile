# ~/.profile: executed by the command interpreter for login shells.
# This may mean that this is read by sh (dash) don't use Bash-isms.
# This file is not read if ~/.bash_profile or ~/.bash_login exists.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# set the most useful XDG user directories explicitly to default
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # per-os setup
    # shellcheck disable=SC3028
    case "$OSTYPE" in
        darwin*)
            # set path, manpath, etc. for homebrew.
            [ -x /opt/homebrew/bin/brew ] &&
                eval "$(/opt/homebrew/bin/brew shellenv)"
            # add clangd/llvm for lsp mode.
            [ -d /opt/homebrew/opt/llvm/bin ] &&
                export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
            # add gnu grep to system to use over bsd grep.
            [ -d /opt/homebrew/opt/grep/libexec/gnubin ] &&
                export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
            [ -d /opt/homebrew/opt/gnu-sed/libexec/gnubin ] &&
               export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"
            ;;
    esac

    # set PATH so it includes user's private bin if it exists
    if [ -d "$HOME/bin" ]; then
        PATH="$HOME/bin:$PATH"
    fi

    if [ -d "$HOME/.local/bin" ]; then
        PATH="$HOME/.local/bin:$PATH"
    fi

    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set emacs as system editor.
export EDITOR="emacs"
export VISUAL="$EDITOR"

# Doom may require this for non-interactive use.
export EMACSDIR="$HOME/.config/emacs"

# Prevent less from using a history file.
export LESSHISTFILE="/dev/null"
