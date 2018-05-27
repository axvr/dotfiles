" =============================================================
" Description:  Improve Perl Script Writing
" File:         ~/.vim/ftdetect/perl.vim
" =============================================================

autocmd BufNewFile *.pl %d|r ~/.vim/skeleton/skel.pl|1d
autocmd BufNewFile *.pm %d|r ~/.vim/skeleton/skel.pm|1d
