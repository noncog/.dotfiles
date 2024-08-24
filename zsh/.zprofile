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

# Add GNU grep to system instead of BSD grep.
PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"

# Add local script binary directory to $PATH.
PATH="$HOME/.local/bin:$PATH"

# Add Clangd/Llvm for LSP mode.
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
