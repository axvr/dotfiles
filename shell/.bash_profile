#!/usr/bin/env bash

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
export VISUAL=nvim
export EDITOR=nvim

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

# Other configs
export FONT_DIR="$HOME/.fonts"

if [ "$(uname -s)" = "Darwin" ]; then
    # Homebrew.
    eval "$(/opt/homebrew/bin/brew shellenv)"

    # Bash completion.
    [[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"
fi

# ASDF
# asdf_install="/opt/homebrew/opt/asdf/libexec/asdf.sh"
# [[ -s "$asdf_install" ]] && . "$asdf_install"
# asdf_completion="/opt/homebrew/opt/asdf/etc/bash_completion.d/asdf.bash"
# [[ -s "$asdf_completion" ]] && . "$asdf_completion"

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
