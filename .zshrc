# Enable command completions
autoload -Uz compinit
compinit
#bashcompinit

# Enable 'line editor'/readline keybinds
bindkey -e # NOTE: Fixed using SKHD and Kitty on macOS to emulate Linux. (Cmd -> Ctrl, Opt -> Alt)

# Aliases
alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

# Sketchybar interactivity overloads
function brew() {
  command brew "$@"

  if [[ $* =~ "upgrade" ]] || [[ $* =~ "update" ]] || [[ $* =~ "outdated" ]]; then
    sketchybar --trigger brew_update
  fi
}

alias lsregister="/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"
