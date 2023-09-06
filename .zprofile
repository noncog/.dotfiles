# Set PATH, MANPATH, etc., for Homebrew.
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Set XDG directories
export XDG_CONFIG_HOME="$HOME/.config"

# Add Doom wrapper script to $PATH.
PATH="$PATH:$HOME/.config/doom/wrapper"

# Set Emacs as system editor.
export EDITOR="emacsclient -c -s $(cat "$HOME"/.config/chemacs/profile) -a=''"
export VISUAL="$EDITOR"

# Add GNU grep to system instead of BSD grep.
PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"

# Add local script binary directory to $PATH.
PATH="$HOME/.local/bin:$PATH"

# Add Clangd/Llvm for LSP mode.
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
