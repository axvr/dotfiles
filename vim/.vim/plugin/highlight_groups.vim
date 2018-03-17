" =============================================================
" Description:  Display Highlight Groups
" File:         ~/.vim/plugin/highlight_groups.vim
" =============================================================

" Show Highlighting group for current word
function! s:SynStack()
    if !exists('*synstack')
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

nnoremap <silent> <Plug>HighlightGroups :<C-u>call <SID>SynStack()<CR>

if empty(maparg('<Leader>hg', 'n'))
    nmap <leader>hg :call <SID>SynStack()<CR>
endif
