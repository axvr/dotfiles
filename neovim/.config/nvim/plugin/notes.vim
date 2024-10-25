function! s:JournalEntry() abort
    let l:date = trim(system(['date', '+%Y-%m-%d']))
    exec 'split | lcd' $NOTES_DIR '| edit' 'Journal/' . l:date . '.md'
endfunction

command -nargs=0 Notes split | lcd $NOTES_DIR | edit $NOTES_DIR
command -nargs=0 Journal call s:JournalEntry()
