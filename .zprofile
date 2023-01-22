# Set PATH, MANPATH, etc., for Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"

export XDG_CONFIG_HOME="$HOME/.config"


# Add Doom to $PATH.
PATH="$PATH:$HOME/.config/emacs/bin"
#
# Set Doom environment variables.
export DOOMDIR="$HOME/.config/doom"
export EMACSDIR="$HOME/.config/emacs"

# Set Emacs as system editor.
export EDITOR=emacs
export VISUAL="$EDITOR"
