" =============================================================
" Description:  Better HTML editing in Vim
" File:         ~/.vim/ftplugin/html.vim
" =============================================================

inoremap </ </<C-x><C-o>

" TODO improve these (especially with tabs and spaces)
inoreabbrev <buffer> _html <!DOCTYPE HTML><CR><CR><html><CR><head><Esc>mho</head><CR><body><CR></body><CR></html><Esc>'ho<BS>
inoreabbrev <buffer> _blog ###TITLE###:<Esc>mto###AUTHOR###:Alex Vear<CR><CR><div class="item"><CR><h3>Date:</h3><CR><CR></div><Esc>'tA
