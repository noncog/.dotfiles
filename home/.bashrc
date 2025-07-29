# ~/.bashrc: executed for non-login shells.
# TODO: Add note on both .profile and .bashrc with compatibility statement.

# return if in a non-interactive shell.
case $- in
    *i*) ;;
    *) return ;;
esac

function settings::global() {
    # NOTE: 1 = true, 0 = false, opposite of Shell return codes.
    # HISTORY SETTINGS
    shopt -s histappend    # append to the history file, don't overwrite it.
    HISTCONTROL=ignoreboth # ignore space prefixed or duplicate lines.
    HISTSIZE=10000         # set history length and file size.
    HISTFILESIZE=8000      # - these values are larger than debian default.

    # BEHAVIOR SETTINGS
    shopt -s checkwinsize # keep window size (LINES and COLUMNS) updated.
    shopt -u globstar     # disable pathname expansion using globs.

    # USER-MADE OPTIONS
    local COLOR_TTY='dracula' # Use selected theme in TTY for Linux.
    local COLOR_CLI=1         # Use color in various program outputs.
    local COLOR_FILE=''       # Use .dircolors or equivalent file for color settings.

    # ALIASES
    [ -x ~/.dotfiles/bin/dotfiles ] \
        && alias dotfiles='~/.dotfiles/bin/dotfiles'
    [ -x ~/.config/emacs/bin/doom ] \
        && alias doom='~/.config/emacs/bin/doom'
    [ -x ~/.local/src/emacs-29/src/emacs ] \
        && alias memacs='~/.local/src/emacs-29/src/emacs --no-site-lisp --no-x-resources --no-site-file --no-splash --init-directory="~/.dotfiles/home/.config/emacs/"'

    alias vim='vim -i \"$XDG_CONFIG_HOME/vim/.viminfo\"'

    alias expand_alias='echo '
    settings::apply
    unset -f settings::apply
}

function settings::apply() {
    # The following enables per-os configurations while
    # avoiding several branches or functions by defining
    # the relevant differences as variables then applying
    # their setting. Only calls once to "$OSTYPE".

    # Initialize variables for per-os configs.
    local dir_brewpath=""
    local bin_kitty=""
    local dir_doom=""
    local bin_bash_completion=""
    local bin_dircolors=""

    # Process universal settings.
    dir_doom="$HOME/.config/doom"

    # Process per-os settings.
    case "$OSTYPE" in
        darwin*)
            if [ -n "$HOMEBREW_PREFIX" ]; then
                dir_brewpath="$HOMEBREW_PREFIX/bin"
                bin_kitty="${dir_brewpath}/kitty"
                bin_bash_completion="$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"
                [ "$COLOR_CLI" -eq 1 ] && export CLI_COLOR=1
                bin_dircolors="$dir_brewpath/gdircolors"
            else
                echo "error homebrew does not appear to be installed. Cannot install.."
            fi
            ;;
        linux-gnu*)
            bin_kitty="/usr/bin/kitty"
            if [ -f /usr/share/bash-completion/bash_completion ]; then
                bin_bash_completion="/usr/share/bash-completion/bash_completion"
            elif [ -f /etc/bash_completion ]; then
                bin_bash_completion="/etc/bash_completion"
            fi
            bin_dircolors="/usr/bin/dircolors"
            ;;
    esac

    # Apply processed settings.

    # Set $TERMINAL.
    [ -x "$bin_kitty" ] \
        && export TERMINAL="$bin_kitty"

    # Set $DOOMDIR.
    [ -d "$dir_doom" ] \
        && export DOOMDIR="$dir_doom"

    # Setup Bash completions.
    if [ "x${BASH_COMPLETION_VERSINFO-}" != x ]; then
        if ! shopt -oq posix; then
            if [ -r "$bin_bash_completion" ]; then
                # shellcheck disable=SC1090
                . "$bin_bash_completion"
            else
                # TODO: Develop temp logging lib with verbosity levels.
                echo "bash completions binary not found"
            fi
        fi
    fi

    # Apply color to output using dircolors.
    if [ "$COLOR_CLI" -eq 1 ]; then
        if [ -x "$bin_dircolors" ]; then
            if [ -n "$COLOR_FILE" ] \
                && [ -r "$COLOR_FILE" ]; then
                eval "$(${bin_dircolors} -b $COLORS_FILE)"
            else
                # NOTE: Consider support for using .dircolors file.
                eval "$(${bin_dircolors} -b)"
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

    # Apply TTY theme.
    if [ -n "$COLOR_TTY" ]; then
        if [ "$TERM" = "linux" ]; then
            case "$COLOR_TTY" in
                dracula)
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
                    ;;
                *)
                    echo "unknown theme: $COLOR_TTY, nothing applied."
                    ;;
            esac
        else
            # if verbose say here, otherwise continue.
            #echo "terminal is not a tty, currently only supports linux tty theming"
            :
        fi
    fi

    settings::prompt
    unset -f settings::prompt
}

function settings::prompt() {
    # --location
    # set variable identifying the chroot you work in for the prompt.
    if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        debian_chroot="$(< /etc/debian_chroot)"
    fi

    # --color
    # if terminal natively supports color, enable colored prompt.
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
    # finally set the prompt to be colored or uncolored.
    if [ "$color_prompt" = yes ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi

    # --title
    # if an xterm set the title to user@host:dir
    case "$TERM" in
        xterm* | rxvt*)
            PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
            ;;
        *) ;;
    esac
}

settings::global
unset -f settings::global
