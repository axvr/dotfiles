#!/usr/bin/env bash

[ -f "$HOME/.profile" ] && . "$HOME/.profile"

export LANG=en_GB.UTF-8

prepend_to() {
    opt="$1"; shift
    for v in "$@"; do
        [[ "${!opt}" =~ "$v" ]] || eval "$opt='$v${!opt+:}${!opt}'"
    done
}

append_to() {
    opt="$1"; shift
    for v in "$@"; do
        [[ "${!opt}" =~ "$v" ]] || eval "$opt='${!opt}${!opt+:}$v'"
    done
}

# User local environment and startup programs
prepend_to PATH "$HOME/.local/bin"
# prepend_to MANPATH "$HOME/.local/share/man"
# prepend_to INFOPATH "$HOME/.local/share/info"

# .NET Core settings
append_to PATH "$HOME/.dotnet/tools"
export DOTNET_CLI_TELEMETRY_OPTOUT=1 ASPNETCORE_ENVIRONMENT=Development

# Go environment
export GOPATH="$HOME/.local/share/go" GOPROXY=direct
prepend_to PATH "$GOPATH/bin"

export PATH MANPATH INFOPATH

# Homebrew
if [ "$(uname -s)" = "Darwin" ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    [[ -r "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh" ]] \
        && . "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"
fi

eval "$(mise activate bash)"

# Source interactive bash config.
if [ -n "$BASH_VERSION" ]; then
    [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi
