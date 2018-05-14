" =============================================================
" Description:  Better HTML editing in Vim
" File:         ~/.vim/ftplugin/html.vim
" =============================================================

inoremap <buffer> </ </<C-x><C-o>

" autocmd! BufWritePre,FileWritePre *-content.html call <SID>update_date()|norm ``
" function! s:update_date()
"     execute '1,6s/<h3>Date:.*/<h3>Date: '.strftime('%Y-%m-%d').'<\/h3>'
" endfunction
