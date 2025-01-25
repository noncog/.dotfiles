# ~/.bashrc: executed for non-login shells.

# return if in a non-interactive shell.
case $- in
    *i*) ;;
      *) return ;;
esac

# HISTORY SETTINGS
# don't put duplicate lines or lines starting with space in the history.
shopt -s histappend    # append to the history file, don't overwrite it.
HISTCONTROL=ignoreboth # ignore space prefixed or duplicate lines.
HISTSIZE=10000         # set history length and file size.
HISTFILESIZE=8000      # - these values are larger than debian default.

# BEHAVIOR SETTINGS
shopt -s checkwinsize  # keep window size (LINES and COLUMNS) updated.
shopt -u globstar      # disable pathname expansion using globs.

# PROMPT SETTINGS
# --location
# set variable identifying the chroot you work in for the prompt.
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot="$(< /etc/debian_chroot)"
fi

# --color
# if terminal natively supports color, enable colored prompt.
# otherwise, check for color capability for terminals that
# support it but don't expose its capability by default.
case "$TERM" in
    xterm-color | *-256color | xterm-kitty)
        color_prompt=yes
        ;;
    *) # Assumes ECMA-48 (ISO/IEC-6429) compliant
        if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
            color_prompt=yes
        else
            color_prompt=
        fi
esac
# finally set the prompt to be colored or uncolored.
if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi

unset color_prompt

# --title
# if an xterm set the title to user@host:dir
case "$TERM" in
    xterm* | rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *) ;;
esac

# COLOR SUPPORT SETTINGS
# enable color for ls and others using dircolors and add handy aliases.
# TODO: Make macOS compatible.
# TODO: Consider a separate variable for .dircolors location.
# TODO: Consider hard-coding DIRCOLORS with means of customizing.
if [ -x /usr/bin/dircolors ]; then
    if [ -r ~/.dircolors ]; then
        eval "$(dircolors -b ~/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
    alias ls='ls --color=auto'
    alias lsa='ls -a --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias rg='rg --color=auto'
else
    alias lsa='ls -a'
fi

# enable colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
# TODO: Check if completions works on macos.
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# enable themes in TTY, specifically the Dracula theme.
# TODO: Consider making this selectable and support others.
if [ "$TERM" = "linux" ]; then
    printf %b '\e[40m' '\e[8]' # set default background to color 0 'dracula-bg'
    printf %b '\e[37m' '\e[8]' # set default foreground to color 7 'dracula-fg'
    printf %b '\e]P0282a36'    # redefine 'black'          as 'dracula-bg'
    printf %b '\e]P86272a4'    # redefine 'bright-black'   as 'dracula-comment'
    printf %b '\e]P1ff5555'    # redefine 'red'            as 'dracula-red'
    printf %b '\e]P9ff7777'    # redefine 'bright-red'     as '#ff7777'
    printf %b '\e]P250fa7b'    # redefine 'green'          as 'dracula-green'
    printf %b '\e]PA70fa9b'    # redefine 'bright-green'   as '#70fa9b'
    printf %b '\e]P3f1fa8c'    # redefine 'brown'          as 'dracula-yellow'
    printf %b '\e]PBffb86c'    # redefine 'bright-brown'   as 'dracula-orange'
    printf %b '\e]P4bd93f9'    # redefine 'blue'           as 'dracula-purple'
    printf %b '\e]PCcfa9ff'    # redefine 'bright-blue'    as '#cfa9ff'
    printf %b '\e]P5ff79c6'    # redefine 'magenta'        as 'dracula-pink'
    printf %b '\e]PDff88e8'    # redefine 'bright-magenta' as '#ff88e8'
    printf %b '\e]P68be9fd'    # redefine 'cyan'           as 'dracula-cyan'
    printf %b '\e]PE97e2ff'    # redefine 'bright-cyan'    as '#97e2ff'
    printf %b '\e]P7f8f8f2'    # redefine 'white'          as 'dracula-fg'
    printf %b '\e]PFffffff'    # redefine 'bright-white'   as '#ffffff'
    clear
fi

# NON-LOGIN ENVIRONMENT VARIABLES
# set TERMINAL
case "$OSTYPE" in
    darwin*)
        [ -x /opt/homebrew/bin/kitty ] &&
            export TERMINAL="/opt/homebrew/bin/kitty"

       ;;
    linux-gnu*)
        [ -x /usr/bin/kitty ] &&
            export TERMINAL="/usr/bin/kitty"
        ;;
esac

# set doom emacs' variables.
# NOTE: Runs Doom from .dotfiles directory to prevent issues with symlinks
#       on macOS with Doom.
# NOTE: Could overwrite EDITOR and VISUAL here to allow ease of changing
#       which Emacs version is used.
# NOTE: Requires using $EMACS_SERVER_NAME and $EMACS_SOCKET_NAME for
#       emacsclient with non-standard install location of Doom.
export EMACSDIR="$HOME/.local/share/doom-emacs"
export DOOMDIR="$HOME/.dotfiles/home/.config/doom"

# ALIASES
# dotfiles and editor management
[ -x ~/.dotfiles/bin/dotfiles ] &&
    alias dotfiles='~/.dotfiles/bin/dotfiles'

[ -x ~/.local/share/doom-emacs/bin/doom ] &&
    alias doom='~/.local/share/doom-emacs/bin/doom'
