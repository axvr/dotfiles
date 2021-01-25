#!/bin/sh

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
PATH="${PATH}:$HOME/.local/bin"
MANPATH="${MANPATH}:$HOME/.local/man"

# Golang environment
export GOPATH="$HOME/.local/share/go"

# .NET Core settings
PATH="${PATH}:$HOME/.dotnet/tools"
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1

export PATH MANPATH
