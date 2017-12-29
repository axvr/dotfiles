" =============================================================
" Description:  Set up Vim for editing Ledger files
" File:         ~/.vim/ftplugin/ledger.vim
" =============================================================

if g:loaded_vivid == 1 && vivid#enabled('vim-ledger') != 1
    call vivid#enable('vim-ledger') | edit
endif

