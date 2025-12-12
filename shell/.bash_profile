# ~/.bash_profile - Bash "login" (non-interactive/set-once) config.

[ -f "$HOME/.profile" ] && . "$HOME/.profile"

prepend_to() {
    local opt="$1"; shift; for v in "$@"; do
        [[ "${!opt}" =~ "$v" ]] || eval "$opt='$v${!opt+:}${!opt}'"
    done
}
append_to() {
    local opt="$1"; shift; for v in "$@"; do
        [[ "${!opt}" =~ "$v" ]] || eval "$opt='${!opt}${!opt+:}$v'"
    done
}

prepend_to PATH "$HOME/.local/bin"
# prepend_to MANPATH "$HOME/.local/share/man"
# prepend_to INFOPATH "$HOME/.local/share/info"

export DOTNET_CLI_TELEMETRY_OPTOUT=1 ASPNETCORE_ENVIRONMENT=Development
append_to PATH "$HOME/.dotnet/tools"

export GOPATH="$HOME/.local/share/go" GOPROXY=direct
append_to PATH "$GOPATH/bin"

export PATH MANPATH INFOPATH LANG=en_GB.UTF-8

export ERL_AFLAGS="-kernel shell_history enabled"
export ELIXIR_ERL_OPTIONS="-kernel shell_history enabled"

[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
[ "$(command -v mise)" ] && eval "$(mise activate bash)"

# Source interactive Bash script if Bash login shell is started interactively.
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
