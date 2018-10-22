" Single file plugin manager

" TODO provide a default path (~/.vim/pack/fileman/start/<dir>/<filename>.vim)
" TODO windows support, and check if curl is installed (maybe replace curl)
" TODO allow performing regex actions on the file?
" TODO FileClean (removes the pack/file/start/ directory)

function! s:File(url, local) abort
    " let s:default_install_dir = expand('~/.vim/pack/fileman/start/fileman/')
    let l:url = a:url
    if l:url !~? '\m\C^\w\+:\/\/.*$'
        let l:url = 'https://raw.githubusercontent.com/'.l:url
    endif
    if !filereadable(expand(a:local)) && executable('curl')
        let l:cmd = 'curl --create-dirs "'.l:url.'" -o "'.expand(a:local).'"'
        call system(l:cmd)
        echomsg "Installed:" a:local
    endif
endfunction

command! -nargs=+ -bar File :call s:File(<args>)
