if status is-interactive
    set fish_greeting  # Hide greeting.

    # Improve colours in prompt.
    set fish_color_cwd blue
    set -g __fish_git_prompt_showcolorhints 1
end

set DEFAULT_VIM 'nvim'

set --export VISUAL $DEFAULT_VIM
set --export EDITOR $DEFAULT_VIM
set --export MANPAGER "less --RAW-CONTROL-CHARS --use-color --color=d+y --color=u+R"

abbr --add -- vi vim
alias ex 'vim -E'
alias vim $DEFAULT_VIM
alias svim 'vim -S Session.vim'
if [ $DEFAULT_VIM = 'nvim' ]; alias vimdiff 'nvim -d'; end

set --export LEDGER_FILE "$HOME/Documents/Ledger/Personal.journal"
alias ledger 'hledger'
alias ledger-record '$EDITOR "$LEDGER_FILE"'
alias ledger-web 'echo "http://127.0.0.1:5000" | open-urls && hledger web -- --serve'

set --export NOTES_DIR "$HOME/Documents/Notes"
alias notes 'vim +Notes +tabonly'

# Aliases to fix files on Apple computers.
alias fix_dir 'chmod a=rx,u=rwx'
alias fix_file 'chmod a=r,u=rw'
alias unquarantine 'xattr -d com.apple.quarantine'
