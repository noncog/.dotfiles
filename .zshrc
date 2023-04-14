alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Enable command completions
autoload -Uz compinit
compinit

# Enable 'line editor'/readline keybinds
bindkey -e # TODO: Fix keys

# Aliases
alias emacs="emacsclient -c"
