# ~/.profile

# Source `.bashrc` if Bash is the default shell
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# Set locale and language
export LANG=en_GB.UTF-8
# export LANG=en_US.UTF-8
# export LC_ALL=POSIX

# Set default editor
export VISUAL=vim
export EDITOR=vim

# User specific environment and startup programs
PATH="${PATH}:$HOME/.local/bin:$HOME/.dotnet/tools"
export PATH

# .NET Core settings
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# Plan 9 from User Space
PLAN9="$HOME/Documents/Projects/plan9port" export PLAN9
PATH="${PATH}:$PLAN9/bin" export PATH

# Start window manager on login in TTY1
if [ -z "$DISPLAY" ] && [[ "$(tty)" = /dev/tty1 ]]; then
    if [ $(command -v startx) ]; then
        exec startx && vlock
    elif [ $(command -v wayland) ]; then
        exec wayland && vlock
    fi
fi
