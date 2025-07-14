" Load additional project-local config into Vim.  My standard file for this is
" a ".axvr.vim" file.  Only automatically loads "trusted" config files.

let s:trust_store = expand($HOME . '/.vim/trust/')
call mkdir(s:trust_store, 'p')

function! s:trust(checksum) abort
    return writefile([], s:trust_store . a:checksum)
endfunction

function! s:is_trusted(checksum) abort
    return filereadable(s:trust_store . a:checksum)
endfunction

function! s:warn(msg) abort
    echohl WarningMsg | echomsg a:msg | echohl NONE
endfunction

function! s:load_local_config(file) abort
    if filereadable(a:file)
        let checksum = sha256(string(readfile(a:file)))
        if s:is_trusted(checksum)
            execute 'source' a:file
            call s:warn('Loaded local config from: "' . a:file . '".')
        else
            call s:warn('Found untrusted local config file: "' . a:file . '".')
            if confirm('Trust and load this file?', "&Yes\n&No", 0, 'Question') == 1
                call s:trust(checksum)
                call s:warn('Marked "' . a:file . '" as trusted.')
                call s:load_local_config(a:file)
            endif
        endif
    endif
endfunction

augroup AxvrPreload
    autocmd!
    autocmd VimEnter * call s:load_local_config('.axvr.vim')
augroup END
