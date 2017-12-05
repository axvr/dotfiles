" =============================================================
" Description:  Set up Vim for Editing Perl Files
" File:         ~/.vim/ftplugin/perl.vim
" =============================================================

" Set up Vim make and quickfix
if executable('perl')
    setlocal makeprg=perl\ -c\ %
    setlocal errorformat+=%m\ at\ %f\ line\ %l\.
    setlocal errorformat+=%m\ at\ %f\ line\ %l
endif

