" Summary: Load additional project-local config into Vim.
" Help:    :help axvr/local-config

function! s:load_local_config(file) abort
    if filereadable(a:file)
        let checksum = sha256(string(readfile(a:file)))
        if axvr#IsTrusted(checksum)
            execute 'source' a:file
            call axvr#Warn('Loaded local config from: "' .. a:file .. '".')
        else
            call axvr#Warn('Found untrusted local config file: "' .. a:file .. '".')
            if axvr#YN('Trust and load this file?')
                call axvr#Trust(checksum)
                call axvr#Warn('Marked "' .. a:file .. '" as trusted.')
                call s:load_local_config(a:file)
            endif
        endif
    endif
endfunction

augroup axvr/local-config
    autocmd!
    autocmd VimEnter * call s:load_local_config('.axvr.vim')
augroup END
