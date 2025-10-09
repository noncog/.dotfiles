# ~/.bashrc: executed for non-login shells.
# shellcheck disable=SC1090,SC1091

# Return if this is a non-interactive shell.
case $- in
    *i*) ;;
    *) return ;;
esac

# Configuration is done inside of functions to allow for the use of local
# variables and to split the configuration for modularity. Any functions
# defined here are unset before the end of this file's execution.

# The following is the main entry point to the global configuration.
function init::bash() {
    # HISTORY SETTINGS
    shopt -s histappend    # append to the history file, don't overwrite it.
    HISTCONTROL=ignoreboth # ignore space prefixed or duplicate lines.
    HISTSIZE=10000         # set history length and file size.
    HISTFILESIZE=8000      # - these values are larger than debian default.

    # BEHAVIOR SETTINGS
    shopt -s checkwinsize # keep window size (LINES and COLUMNS) updated.
    shopt -u globstar     # disable pathname expansion using globs.

    # USER-MADE OPTIONS             Uncomment to use. Set booleans to 'true' or 'false'.
    local COLOR_TTY='dracula'       # Use selected theme in TTY for Linux.
    local COLOR_CLI='true'          # Use color in various program outputs.
    local COLOR_FILE=''             # Use .dircolors or equivalent file for colors.
    local TERMINAL='/usr/bin/kitty' # Use this terminal for i3 and rofi on Linux.

    # ENVIRONMENT VARIABLES
    [[ "${XDG_CONFIG_HOME/bash/inputrc}" ]] && export INPUTRC="${XDG_CONFIG_HOME/bash/inputrc}"

    export EMACSDIR="$XDG_CONFIG_HOME/emacs"
    export DOOMDIR="${XDG_CONFIG_HOME}/doom"

    # Prevent $HOME directory pollution.
    export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
    export LESSHISTFILE="/dev/null"

    # ALIASES
    alias expand_alias='echo '
    alias lsa='ls -a'
    alias cp='cp -vi'
    alias mv='mv -vi'

    # Prevent $HOME directory pollution.
    alias vim='vim -i \"$XDG_CONFIG_HOME/vim/.viminfo\"'

    if [[ -x ~/.dotfiles/bin/dotfiles ]]; then
        alias dotfiles='~/.dotfiles/bin/dotfiles'
    fi

    [[ -x ~/.config/emacs/bin/doom ]] \
        && alias doom='~/.config/emacs/bin/doom'

    [[ -d ~/.dotfiles/home/.config/emacs/ ]] \
        && alias memacs='emacs --no-site-lisp --no-x-resources --no-site-file --no-splash --init-directory="~/.dotfiles/home/.config/emacs/"'

    # CONFIGURATION FUNCTIONS
    init::per_os
    unset -f init::per_os
    init::prompt
    unset -f init::prompt
}

function init::per_os() {
    # Process per-os differences.
    local bin_bash_completion bin_dircolors

    case "$OSTYPE" in
        darwin*)
            bin_bash_completion="$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"
            bin_dircolors="${HOMEBREW_PREFIX}/bin/gdircolors"
            ;;
        linux-gnu*)
            bin_bash_completion="/usr/share/bash-completion/bash_completion"
            bin_dircolors="/usr/bin/dircolors"
            # Set $TERMINAL variable used by: [i3|rofi]-sensible-terminal
            [[ -x "$TERMINAL" ]] && export TERMINAL
            ;;
    esac

    # Setup Bash completions.
    if [ "x${BASH_COMPLETION_VERSINFO-}" != x ] && ! shopt -oq posix; then
        [[ -r "$bin_bash_completion" ]] && source "$bin_bash_completion"
    fi

    init::color
    unset -f init::color
}
function init::color() {
    # LS_COLORS may be called LSCOLORS
    # https://www.pixelbeat.org/scripts/l
    # https://ss64.com/bash/lsenv.html
    # NOTE: Consider support for using .dircolors file.

    # Apply color to output using dircolors.
    if "$COLOR_CLI"; then
        export CLI_COLOR=1

        if [[ -x "$bin_dircolors" ]]; then
            if [[ -n "$COLOR_FILE" ]] && [[ -r "$COLOR_FILE" ]]; then
                eval "$(${bin_dircolors} -b "$COLOR_FILE")"
            else
                eval "$(${bin_dircolors} -b)"
            fi
            alias ls='ls --color=auto'
            alias lsa='ls -a --color=auto'
            alias dir='dir --color=auto'
            alias vdir='vdir --color=auto'
            alias grep='grep --color=auto'
            alias fgrep='fgrep --color=auto'
            alias egrep='egrep --color=auto'
            alias rg='rg --color=auto'
        fi
        # enable colored GCC warnings and errors
        export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
    fi

    init::color_tty
    unset -f init::color_tty
}

function init::color_tty() {
    # Apply the TTY color theme if enabled. More themes can be added here.
    if [[ -n "$COLOR_TTY" ]] && [[ "$TERM" = "linux" ]]; then
        case "$COLOR_TTY" in
            dracula)
                printf '%b' '\e[40m' '\e[8]' # set default background to color 0 'dracula-bg'
                printf '%b' '\e[37m' '\e[8]' # set default foreground to color 7 'dracula-fg'
                printf '%b' '\e]P0282a36'    # 'black'          as 'dracula-bg'
                printf '%b' '\e]P86272a4'    # 'bright-black'   as 'dracula-comment'
                printf '%b' '\e]P1ff5555'    # 'red'            as 'dracula-red'
                printf '%b' '\e]P9ff7777'    # 'bright-red'     as '#ff7777'
                printf '%b' '\e]P250fa7b'    # 'green'          as 'dracula-green'
                printf '%b' '\e]PA70fa9b'    # 'bright-green'   as '#70fa9b'
                printf '%b' '\e]P3f1fa8c'    # 'brown'          as 'dracula-yellow'
                printf '%b' '\e]PBffb86c'    # 'bright-brown'   as 'dracula-orange'
                printf '%b' '\e]P4bd93f9'    # 'blue'           as 'dracula-purple'
                printf '%b' '\e]PCcfa9ff'    # 'bright-blue'    as '#cfa9ff'
                printf '%b' '\e]P5ff79c6'    # 'magenta'        as 'dracula-pink'
                printf '%b' '\e]PDff88e8'    # 'bright-magenta' as '#ff88e8'
                printf '%b' '\e]P68be9fd'    # 'cyan'           as 'dracula-cyan'
                printf '%b' '\e]PE97e2ff'    # 'bright-cyan'    as '#97e2ff'
                printf '%b' '\e]P7f8f8f2'    # 'white'          as 'dracula-fg'
                printf '%b' '\e]PFffffff'    # 'bright-white'   as '#ffffff'
                clear
                ;;
        esac
    fi
}

function init::prompt() {
    # Setup the prompt according to system capabilities.

    # LOCATION
    # Set variable identifying the chroot you work in for the prompt.
    # NOTE: Only setup to work with Debian for now.
    if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        debian_chroot="$(< /etc/debian_chroot)"
    fi

    # COLOR
    # If terminal natively supports color, enable colored prompt.
    # otherwise, check for color capability for terminals that
    # support it but don't expose its capability by default.
    local color_prompt
    case "$TERM" in
        xterm-color | *-256color | xterm-kitty)
            color_prompt=yes
            ;;
        *) # Assumes ECMA-48 (ISO/IEC-6429) compliant
            if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
                color_prompt=yes
            else
                color_prompt=
            fi ;;
    esac

    # Setup the prompt, colored or uncolered.
    if [ "$color_prompt" = yes ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi

    # TITLE
    # If an xterm set the title to user@host:dir
    case "$TERM" in
        xterm* | rxvt*)
            PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
            ;;
        *) ;;
    esac
}

init::bash
unset -f init::bash
