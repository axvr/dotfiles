" =============================================================
" Description:  Set up Vim for Editing Perl Files
" File:         ~/.vim/ftplugin/perl.vim
" =============================================================

setlocal textwidth=80

" Set up Vim make and quickfix
if executable('perl')
    compiler perl
endif
