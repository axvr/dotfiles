function! argbook#Jump(name) abort
    if !empty(a:name)
        " FIXME: broken name normalisation breaks jumping.
        " TODO: custom error messgae if not found.
        exec 'argument' 1 + index(argv()->map({_, v -> fnamemodify(v, ':p:.')}), a:name)
    else
        call axvr#Warn('argbook#Jump: No file under cursor.')
    endif
endfunction

function! argbook#Load() abort
    %delete _
    call argv()->map({_, v -> fnamemodify(v, ':p:.')})
              \->map({i, l -> setbufline(bufnr('%'), i + 1, l)})
    setlocal nomodified
endfunction

" TODO: maintain previously selected arg?  I.e. delete/replace around it.
function! argbook#Write() abort
    %argdelete
    for arg in getbufline(bufnr('%'), 1, '$')
        if !empty(arg)
            exec '$argadd' fnameescape(fnamemodify(arg, ':.'))
        endif
    endfor
    setlocal nomodified
endfunction

let s:arglist_bufnr = 0

function! argbook#Open() abort
    let arglistid = arglistid()
    let winid = win_getid()

    if s:arglist_bufnr && bufexists(s:arglist_bufnr)
        let bufnr = s:arglist_bufnr
    else
        let bufnr = bufadd('[Argument List]')
        call setbufvar(bufnr, '&filetype', 'argbook')
        call bufload(bufnr)
        let s:arglist_bufnr = bufnr
    endif

    " TODO: jump to in current tab if already open.
    " TODO: apply <q-mods>
    exec 'buffer' bufnr
endfunction

function! argbook#Reload() abort
    set nobuflisted  " Vim will enable this each time the buffer is edited.
    if !&l:modified | call argbook#Load() | endif
endfunction
