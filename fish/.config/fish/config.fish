if status is-interactive
    # Commands to run in interactive sessions can go here

    # Improve colours in prompt.
    set fish_color_cwd blue
    set -g __fish_git_prompt_showcolorhints 1
end


# Set locale and language
set --export LANG 'en_GB.UTF-8'
# set --export LC_ALL 'POSIX'

# Set default editor
set --export VISUAL 'nvim'
set --export EDITOR 'nvim'

fish_add_path --prepend "$HOME/.local/bin"

# .NET
fish_add_path --append "$HOME/.dotnet/tools"
set --export ASPNETCORE_ENVIRONMENT 'Development'
set --export DOTNET_CLI_TELEMETRY_OPTOUT 1

# Go
set --export GOPROXY 'direct'
set --export GOPATH "$HOME/.local/share/go"
fish_add_path --append "$GOPATH/bin"

# Other
set --export FONT_DIR "$HOME/.fonts"
set --export MANPAGER "less --RAW-CONTROL-CHARS --use-color --color=d+y --color=u+R"

# Homebrew
if [ "(uname -s)" = 'Darwin' ]
    eval "(/opt/homebrew/bin/brew shellenv)"
end

abbr --add -- vi nvim
abbr --add -- vim nvim
alias svim 'nvim -S Session.vim'
alias vimdiff 'nvim -d'

set --export LEDGER_FILE "$HOME/Documents/Ledger/Personal.journal"
alias ledger 'hledger'
alias ledger-record '$EDITOR "$LEDGER_FILE"'
alias ledger-web 'echo "http://127.0.0.1:5000" | open-urls && hledger web -- --serve'

alias rtx 'mise'
alias sdk 'mise'
alias asdf 'mise'

# Aliases to fix files on Apple computers.
alias fix_dir 'chmod a=rx,u=rwx'
alias fix_file 'chmod a=r,u=rw'
alias unquarantine 'xattr -d com.apple.quarantine'
