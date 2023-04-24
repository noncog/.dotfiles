# Set PATH, MANPATH, etc., for Homebrew.
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Set XDG directories
export XDG_CONFIG_HOME="$HOME/.config"

# Add Doom wrapper script to $PATH.
PATH="$PATH:$HOME/.config/doom/wrapper"

# Set Doom environment variables.
export DOOMDIR="$HOME/.config/doom"
export EMACSDIR="$HOME/.config/emacs"

# Set Emacs as system editor.
export EDITOR="emacsclient -c -a emacs"
export VISUAL="emacsclient -c -a emacs"

# Add GNU grep to system instead of BSD grep.
PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"

# Add Clangd/Llvm for LSP mode.
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
