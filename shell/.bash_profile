#!/usr/bin/env bash

# ~/.bash_profile

# Source `.bashrc` if Bash is the default shell
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# Set locale and language
export LANG=en_GB.UTF-8
# export LC_ALL=POSIX

# Set default editor
export VISUAL=vim
export EDITOR=vim

prepend_to() {
    opt="$1"
    shift
    for v in "$@"; do
        if ! [[ "${!opt}" =~ "$v" ]]; then
            eval "$opt='$v${!opt+:}${!opt}'"
        fi
    done
}

append_to() {
    opt="$1"
    shift
    for v in "$@"; do
        if ! [[ "${!opt}" =~ "$v" ]]; then
            eval "$opt='${!opt}${!opt+:}$v'"
        fi
    done
}

# User local environment and startup programs
prepend_to PATH "$HOME/.local/bin"
# prepend_to MANPATH "$HOME/.local/share/man"
# prepend_to INFOPATH "$HOME/.local/share/info"

# .NET Core settings
append_to PATH "$HOME/.dotnet/tools"
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# Go environment
export GOPROXY=direct
export GOPATH="$HOME/.local/share/go"
prepend_to PATH "$GOPATH/bin"

export PATH MANPATH INFOPATH
