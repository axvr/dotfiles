# ~/.config/fish/config.fish

if status is-interactive
    set fish_greeting  # Hide greeting.

    # Improve colours in prompt.
    set fish_color_cwd blue
    set -g __fish_git_prompt_showcolorhints 1

    set --export EDITOR 'nvim'
    set --export VISUAL $EDITOR
    abbr --add -- vi vim
    alias ex 'vim -E'
    alias svim 'vim -S Session.vim'
    if test 'nvim' = $EDITOR
        alias vim 'nvim'
        alias vimdiff 'nvim -d'
    end

    set --export MANPAGER 'less --RAW-CONTROL-CHARS --use-color --color=d+y --color=u+R'

    set --export LEDGER_FILE "$HOME/Documents/Ledger/Personal.journal"
    alias ledger 'hledger'
    alias ledger-record '$EDITOR $LEDGER_FILE'
    alias ledger-web 'echo "http://127.0.0.1:5000" | open-urls && hledger web -- --serve'

    set --export NOTES_DIR "$HOME/Documents/Notes"
    alias notes 'vim +Notes +tabonly'

    # Aliases to fix files on Apple computers.
    alias unquarantine 'xattr -d com.apple.quarantine'
    alias fix_dir 'chmod a=rx,u=rwx'
    alias fix_file 'chmod a=r,u=rw'
end
