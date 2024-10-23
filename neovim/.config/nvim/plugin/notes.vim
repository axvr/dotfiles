function! s:JournalEntry() abort
    let l:date = trim(system(['date', '+%Y-%m-%d']))
    exec 'tabe' $NOTES_DIR . '/Journal/' . l:date . '.md'
endfunction

command -nargs=0 Notes tabe $NOTES_DIR
command -nargs=0 Journal call s:JournalEntry()
