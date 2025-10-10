" Easy scratch buffer management and creation.
"
" Help:
"   :Scratch     ->  list scratch buffers.
"   :Scratch ft  ->  open new scratch buffer for filetype.
"   :hori Sc ft  ->  open new scratch buffer for filetype in horizontal split.
"   :vert Sc ft  ->  open new scratch buffer for filetype in vertical split.
"   :tab Sc ft   ->  open new scratch buffer for filetype in a new tab.
"   :Redirect c  ->  redirect output of Vim command `c` to the current buffer.
"
" Tips:
"   - Open existing scratch buffers with standard commands like `:sb` and `b`.
"   - Save contents of scratch buffers with standard commands like `:w file`.

function! s:scratch(mods, filetype) abort
    if empty(a:filetype)
        echo 'Scratch buffers:'
        filter /\M^[Scratch]/ buffers
    else
        let bufnr = bufadd('[Scratch] ' . a:filetype)
        call setbufvar(bufnr, '&filetype', a:filetype)
        if empty(a:mods)
            exec 'buffer' bufnr
        else
            exec a:mods 'sbuffer' bufnr
        endif
        setlocal buftype=nofile noswapfile buflisted
    endif
endfunction

command! -nargs=? -bar -complete=filetype Scratch call s:scratch(<q-mods>, <q-args>)

" Custom version of <https://gist.github.com/romainl/eae0a260ab9c135390c30cd370c20cd7>
command! -nargs=+ -complete=command -bar Redirect call appendbufline(bufnr(),
            \ getcurpos('.')[1] - 1, split(execute(<q-args>), "\n"))
