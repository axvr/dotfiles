" Summary: Notes in Vim.
" Help:    N/A

" TODO: finish this.

command! -nargs=0 -bar Notes
            \ <mods> tabedit $NOTES_DIR | silent lcd $NOTES_DIR | arglocal

function! s:GetConf(name, default) abort
    return get(b:, a:name, get(g:, a:name, a:default))
endfunction

function! s:GetCurrentLink() abort
  let link_regex = s:GetConf('axvr#link_regex', '\[[^]]*\](\zs[^)]\+\ze)')
  " TODO: <cfile>, get org mode regex from my earlier config, <a href="...">...</a>
  return matchstr(getline('.'),
              \ '\%<'.(col('.')+1).'c'..link_regex..'\%>'.col('.').'c')
endfunction

function! s:LinkVariants(link) abort
    let root = (a:link =~# '^/' ? '.' : expand('%:.:h')) .. '/'
    let link = fnamemodify(a:link, ':s?^/??:s?/$??:.')
    let base = root .. link
    return [base .. '.md', base .. '/index.md', base .. '/', base]->map('fnamemodify(v:val, ":p:.")')
endfunction

function! FollowLink() abort
    let f = s:LinkVariants(s:GetCurrentLink())->filter('filereadable(v:val)')->get(0, '')
    if f ==# ''
        call axvr#Warn('File not found.')
    else
        exec 'edit' f
    endif
endfunction

" file -> index.* | README.* | . -> ../index.* | ../README.* | ../
function! GoUp() abort
    let buf = fnamemodify(expand('%'), ':p:.')
    let par = fnamemodify(buf, ':h')
    let edit = has(':Dirvish') ? 'Dirvish' : 'edit'

    let Search = { d -> glob(escape(d, '?*[]') .. '/{index,README}.*', 0, 1)
                      \ ->map('fnamemodify(v:val, ":p:.")') }

    for par in [par, fnamemodify(par, ':h')]
        let fs = Search(par)
        if empty(fs)
            exec edit fnamemodify(par, ':h') | return
        elseif fs[0] ==# buf
            let par = fnamemodify(par, ':h')
        else
            exec 'edit' fs[0] | return
        endif
    endfor

    echo 'Already at root.'
endfunction

augroup axvr/links
    au!
    autocmd FileType markdown nnoremap <silent> <buffer> gf :call FollowLink()<CR>
    autocmd FileType markdown nmap <buffer> <2-LeftMouse> gf
    autocmd FileType markdown,dirvish nnoremap <silent> <buffer> <localleader>u :call GoUp()<CR>
augroup END

" TODO: global mappings.  <leader>lu ???
" TODO: work with other file extensions too.
" TODO: create file if no exist.
