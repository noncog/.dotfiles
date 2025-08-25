# ~/.bashrc: executed for non-login shells.

# Return if this is a non-interactive shell.
case $- in
    *i*) ;;
    *) return ;;
esac

# Configuration is done inside of functions to allow for the use of local
# variables and to split the configuration for modularity. Any functions
# defined here are unset before the end of this file's execution.

# The following is the main entry point to the global configuration.
function settings::bash() {
    # HISTORY SETTINGS
    shopt -s histappend    # append to the history file, don't overwrite it.
    HISTCONTROL=ignoreboth # ignore space prefixed or duplicate lines.
    HISTSIZE=10000         # set history length and file size.
    HISTFILESIZE=8000      # - these values are larger than debian default.

    # BEHAVIOR SETTINGS
    shopt -s checkwinsize # keep window size (LINES and COLUMNS) updated.
    shopt -u globstar     # disable pathname expansion using globs.

    # USER-MADE OPTIONS       Uncomment to use. Set booleans to 'true' or 'false'.
    local COLOR_TTY='dracula' # Use selected theme in TTY for Linux.
    local COLOR_CLI='true'    # Use color in various program outputs.
    local COLOR_FILE=''       # Use .dircolors or equivalent file for colors.

    # ALIASES
    alias vim='vim -i \"$XDG_CONFIG_HOME/vim/.viminfo\"'
    alias expand_alias='echo '

    [ -x ~/.dotfiles/bin/dotfiles ] \
        && alias dotfiles='~/.dotfiles/bin/dotfiles'
    [ -x ~/.config/emacs/bin/doom ] \
        && alias doom='~/.config/emacs/bin/doom'
    [ -x ~/.local/src/emacs-30/src/emacs ] \
        && alias memacs='emacs --no-site-lisp --no-x-resources --no-site-file --no-splash --init-directory="~/.dotfiles/home/.config/emacs/"'

    # GENERAL

    # Set $DOOMDIR.
    [ -d "$HOME/.config/doom" ] \
        && export DOOMDIR="$HOME/.config/doom"

    # CONFIGURATION FUNCTIONS
    settings::per_os
    unset -f settings::per_os

    settings::color_tty
    unset -f settings::color_tty

    settings::prompt
    unset -f settings::prompt
}

# The following function exists to reduce startup time and configure settings
# that may vary between macOS and Linux. It avoids multiple forks by only
# checking the '$OSTYPE' once and setting variables according to each.
function settings::per_os() {
    # Initialize variables.
    local homebrew_bin kitty_bin bash_completion_bin dircolors_bin

    # Process per-os settings.
    case "$OSTYPE" in
        darwin*)
            if [ -n "$HOMEBREW_PREFIX" ]; then
                homebrew_bin="$HOMEBREW_PREFIX/bin"
                kitty_bin="${homebrew_bin}/kitty"
                bash_completion_bin="$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"
                [ "$COLOR_CLI" -eq 1 ] && export CLI_COLOR=1
                dircolors_bin="$homebrew_bin/gdircolors"
            else
                echo "error homebrew does not appear to be installed. Cannot install.."
                # Should return here.
            fi
            ;;
        linux-gnu*)
            kitty_bin="/usr/bin/kitty"
            if [ -f /usr/share/bash-completion/bash_completion ]; then
                bash_completion_bin="/usr/share/bash-completion/bash_completion"
            elif [ -f /etc/bash_completion ]; then
                bash_completion_bin="/etc/bash_completion"
            fi
            dircolors_bin="/usr/bin/dircolors"
            ;;
    esac

    # Apply color to output using dircolors.
    if [ "$COLOR_CLI" -eq 1 ]; then
        if [ -x "$dircolors_bin" ]; then
            if [ -n "$COLOR_FILE" ] \
                && [ -r "$COLOR_FILE" ]; then
                eval "$(${dircolors_bin} -b "$COLOR_FILE")"
            else
                # NOTE: Consider support for using .dircolors file.
                eval "$(${dircolors_bin} -b)"
                alias ls='ls --color=auto'
                alias lsa='ls -a --color=auto'
                alias dir='dir --color=auto'
                alias vdir='vdir --color=auto'
                alias grep='grep --color=auto'
                alias fgrep='fgrep --color=auto'
                alias egrep='egrep --color=auto'
                alias rg='rg --color=auto'
            fi
        else
            alias lsa='ls -a'
        fi
    else
        # enable colored GCC warnings and errors
        export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
    fi

    # Set $TERMINAL.
    [ -x "$kitty_bin" ] \
        && export TERMINAL="$kitty_bin"

    # Setup Bash completions.
    if [ "x${BASH_COMPLETION_VERSINFO-}" != x ]; then
        if ! shopt -oq posix; then
            if [ -r "$bash_completion_bin" ]; then
                # shellcheck disable=SC1090
                . "$bash_completion_bin"
            else
                # TODO: Develop temp logging lib with verbosity levels.
                echo "bash completions binary not found"
            fi
        fi
    fi

}

function settings::color_tty() {
    # Apply the TTY color theme if enabled. More themes can be added here.
    # TODO: Consider refactoring disabling of COLOR_TTY if we can detect earlier that not in one.
    if [ -n "$COLOR_TTY" ]; then
        if [ "$TERM" = "linux" ]; then
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
                *)
                    echo "unknown theme: $COLOR_TTY, nothing applied."
                    ;;
            esac
        else
            # NOTE: Disabled to prevent error message showing in regular graphical terminal session.
            #echo "COLOR_TTY setting: $COLOR_TTY, not applied. TERM: $TERM is unsupported."
            :
        fi
    fi
}

function settings::prompt() {
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

settings::bash
unset -f settings::bash
