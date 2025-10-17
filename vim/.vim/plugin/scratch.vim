" Summary: Easy scratch buffer management and creation.
" Help:    :help axvr/scratch

function! s:scratch(mods, filetype) abort
    if empty(a:filetype)
        echo 'Scratch buffers:'
        filter /\M^[Scratch]/ buffers
    else
        let bufnr = bufadd('[Scratch] ' .. a:filetype)
        call setbufvar(bufnr, '&filetype', a:filetype)
        exec a:mods .. (empty(a:mods) ? ' buffer ' : ' sbuffer ') .. bufnr
        setlocal buftype=nofile noswapfile buflisted
    endif
endfunction

command! -nargs=? -bar -complete=filetype Scratch call s:scratch(<q-mods>, <q-args>)
command! -nargs=+ -complete=command Redir
            \ call appendbufline(bufnr(), getcurpos('.')[1] - 1, split(execute(<q-args>), "\n"))
