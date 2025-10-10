" Easy scratch buffer management and creation.
"
" Help:
"   :Scratch     ->  list scratch buffers.
"   :Scratch ft  ->  open new scratch buffer for filetype.
"   :hori Sc ft  ->  open new scratch buffer for filetype in horizontal split.
"   :vert Sc ft  ->  open new scratch buffer for filetype in vertical split.
"   :tab Sc ft   ->  open new scratch buffer for filetype in a new tab.
"   :Redir cmd   ->  redirect output of Vim command to the current buffer.
"
" Tip: use standanrd Vim commands!
"   - Open existing scratch buffers with `:sbuffer b` and `:buffer b`.
"   - Save scratch buffers with `:write file`.
"   - Load files into scratch buffer with `:read file`

function! s:scratch(mods, filetype) abort
    if empty(a:filetype)
        echo 'Scratch buffers:'
        filter /\M^[Scratch]/ buffers
    else
        let bufnr = bufadd('[Scratch] ' . a:filetype)
        call setbufvar(bufnr, '&filetype', a:filetype)
        exec a:mods . (empty(a:mods) ? ' buffer ' : ' sbuffer ') . bufnr
        setlocal buftype=nofile noswapfile buflisted
    endif
endfunction

command! -nargs=? -bar -complete=filetype Scratch call s:scratch(<q-mods>, <q-args>)
command! -nargs=+ -complete=command Redir
            \ call appendbufline(bufnr(), getcurpos('.')[1] - 1, split(execute(<q-args>), "\n"))
