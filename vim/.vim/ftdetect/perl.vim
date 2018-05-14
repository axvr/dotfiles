" =============================================================
" Description:  Improve Perl Script Writing
" File:         ~/.vim/ftdetect/perl.vim
" =============================================================

autocmd BufNewFile *.pl %d|r ~/.vim/skeleton/skeleton.pl|1d
autocmd BufNewFile *.pm %d|r ~/.vim/skeleton/skeleton.pm|1d
