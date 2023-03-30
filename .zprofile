# Set PATH, MANPATH, etc., for Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"

# Set XDG directories
export XDG_CONFIG_HOME="$HOME/.config"

# Add Doom to $PATH.
PATH="$PATH:$HOME/.config/emacs/bin"

# Set Doom environment variables.
export EMACS="emacsclient -c -a emacs"
export DOOMDIR="$HOME/.config/doom"
export EMACSDIR="$HOME/.config/emacs"

# Set Emacs as system editor.
export EDITOR="emacsclient -c -a emacs"
export VISUAL="emacsclient -c -a emacs"

# Add GNU grep to system instead of BSD grep.
PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
