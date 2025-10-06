# ~/.profile: executed by the command interpreter for login shells.
# - This file may be read by sh (dash), don't use Bash-isms.
# - This file is not read if ~/.bash_profile or ~/.bash_login exist.

# The default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.

# Set environment variables available everywhere:
export EDITOR="emacs"
export VISUAL="$EDITOR"
export SUDO_EDITOR="$EDITOR"

# Set user-specific environment variables:
if [ -d "$HOME" ]; then
    # Set the XDG base directories.
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_CACHE_HOME="$HOME/.cache"
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_STATE_HOME="$HOME/.local/state"
    export XDG_BIN_HOME="$HOME/.local/bin"

    # Prevent $HOME directory pollution.
    export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
    export LESSHISTFILE="/dev/null"

    # If this is Bash...
    if [ -n "$BASH_VERSION" ]; then
        # Setup Homebrew on macOS.
        # shellcheck disable=SC3028,SC3010
        case "$OSTYPE" in
            darwin*)
                if [[ -n "${HOMEBREW_PREFIX-}" ]] \
                    && [[ -x "${HOMEBREW_PREFIX-}/bin/brew" ]]; then
                    # Set path, manpath, etc. for homebrew.
                    eval "$(/opt/homebrew/bin/brew shellenv)"
                    # Add Homebrew package directories to environment.
                    for dir in "${HOMEBREW_PREFIX}/opt/"*"/libexec/gnubin"; do export PATH="$dir:$PATH"; done
                    for dir in "${HOMEBREW_PREFIX}/opt/"*"/bin"; do export PATH="$dir:$PATH"; done
                    for dir in "${HOMEBREW_PREFIX}/opt/"*"/libexec/gnuman"; do export MANPATH="$dir:$MANPATH"; done
                    for dir in "${HOMEBREW_PREFIX}/opt/"*"/share/man/man1"; do export MANPATH="$dir:$MANPATH"; done
                    unset dir
                fi
                ;;
        esac

        # Load Bash configuration.
        [ -r "$HOME/.bashrc" ] && . "$HOME/.bashrc"
    fi
    # Set $PATH to include user's binary directory.
    [ -d "$XDG_BIN_HOME" ] && export PATH="$XDG_BIN_HOME:$PATH"
fi
