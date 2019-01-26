# ~/.bash_profile

# Get the aliases and functions
[ -f "${HOME}/.bashrc" ] && . "${HOME}/.bashrc"

# User specific environment and startup programs
PATH="${PATH}:${HOME}/.local/bin:${HOME}/.bin:${HOME}/.dotnet/tools"
export PATH

# Start window manager on login in TTY1
if [ -z "$DISPLAY" ] && [[ "$(tty)" = /dev/tty1 ]]; then
    if [ $(command -v startx) ]; then
        exec startx
        # exec wayland
        vlock
    fi
fi
