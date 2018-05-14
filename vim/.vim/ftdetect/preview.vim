" =============================================================
" Description:  Better Vim Preview Window
" File:         ~/.vim/ftdetect/preview.vim
" =============================================================

" TODO configure the prview window
" function! s:improved_preview_window() abort
"     if &previewwindow == 1
"         setlocal norelativenumber textwidth=0 nowrap
"         execute 'resize ' line('w$')
"     endif
" endfunction

" TODO detect preview window
" autocmd BufNew * if &previewwindow == 1 | setfiletype preview | endif
