" =============================================================
" Description:  Better HTML editing in Vim
" File:         ~/.vim/ftdetect/html.vim
" =============================================================

autocmd BufNewFile *.html         %d|r ~/.vim/skeleton/skel.html|1d
autocmd BufNewFile *.content.html %d|r ~/.vim/skeleton/skel.content.html|1d
