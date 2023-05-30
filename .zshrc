# Enable command completions
autoload -Uz compinit
compinit
#bashcompinit

# Enable 'line editor'/readline keybinds
bindkey -e # NOTE: Fixed using SKHD on macOS to emulate Linux. (Cmd -> Ctrl, Opt -> Alt)

# Aliases
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=~'
alias emacs='emacsclient -c -n --alternate-editor=""'
