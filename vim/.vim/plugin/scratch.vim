" Scratch buffer

function! s:scratch(mods, bang)
    let b = filter(range(1, bufnr('$')), 'getbufvar(v:val, "scratch")')
    if a:bang ==# '!' || empty(b)
        exec a:mods . ' sbuffer' | enew
        setlocal buftype=nofile noswapfile  " bufhidden=wipe nobuflisted
        let b:scratch = 1
    else
        let swb = &switchbuf
        set switchbuf+=useopen
        exec a:mods . ' sbuffer ' . get(b, 0)
        let &swb = swb
    endif
endfunction

command! -nargs=0 -bar -bang Scratch silent call s:scratch(<q-mods>, <q-bang>)

command! -nargs=+ -complete=command -bar Redirect call appendbufline(bufnr(),
            \ getcurpos('.')[1] - 1, split(execute(<q-args>), "\n"))
