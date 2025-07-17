[ -f "$HOME/.profile" ] && . "$HOME/.profile"

export LANG=en_GB.UTF-8

prepend_to() {
    local opt
    opt="$1"; shift
    for v in "$@"; do
        [[ "${!opt}" =~ "$v" ]] || eval "$opt='$v${!opt+:}${!opt}'"
    done
}

append_to() {
    local opt
    opt="$1"; shift
    for v in "$@"; do
        [[ "${!opt}" =~ "$v" ]] || eval "$opt='${!opt}${!opt+:}$v'"
    done
}

# User programs
prepend_to PATH "$HOME/.local/bin"
# prepend_to MANPATH "$HOME/.local/share/man"
# prepend_to INFOPATH "$HOME/.local/share/info"

# .NET
export DOTNET_CLI_TELEMETRY_OPTOUT=1 ASPNETCORE_ENVIRONMENT=Development
append_to PATH "$HOME/.dotnet/tools"

# Go
export GOPATH="$HOME/.local/share/go" GOPROXY=direct
prepend_to PATH "$GOPATH/bin"

export PATH MANPATH INFOPATH

[ -x "/opt/homebrew/bin/brew" ] && eval "$(/opt/homebrew/bin/brew shellenv)"

eval "$(mise activate bash)"

if [ -n "$BASH_VERSION" ]; then
    # Source interactive bash config.
    [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi
