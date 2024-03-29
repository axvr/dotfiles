" Scratch buffer

function! s:scratch(mods, bang, filetype)
    let b = filter(range(1, bufnr('$')), 'getbufvar(v:val, "scratch")')
    if a:bang ==# '!' || empty(b)
        exec a:mods . ' sbuffer' | enew
        setlocal buftype=nofile noswapfile  " bufhidden=wipe nobuflisted
        if !empty(a:filetype) | exec 'setfiletype ' . a:filetype | endif
        let b:scratch = 1
    else
        let swb = &switchbuf
        set switchbuf+=useopen
        exec a:mods . ' sbuffer ' . get(b, 0)
        let &swb = swb
    endif
endfunction

command! -nargs=? -bar -bang -complete=filetype Scratch silent call s:scratch(<q-mods>, <q-bang>, <q-args>)

" Custom version of <https://gist.github.com/romainl/eae0a260ab9c135390c30cd370c20cd7>
command! -nargs=+ -complete=command -bar Redirect call appendbufline(bufnr(),
            \ getcurpos('.')[1] - 1, split(execute(<q-args>), "\n"))
