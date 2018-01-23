" =============================================================
" Description:  Set up Vim for Editing Encrypted Files
" File:         ~/.vim/ftplugin/encrypted.vim
" =============================================================

" GPG Encrypted Files
" Transparent editing of gpg encrypted files. By Wouter Hanegraaff.
augroup encrypted
    autocmd!
    " First make sure nothing is written to ~/.viminfo while editing
    " an encrypted file.
    autocmd BufReadPre,FileReadPre encrypted setlocal viminfo=
    " We don't want a various options which write unencrypted data to disk
    autocmd BufReadPre,FileReadPre encrypted
                \ setlocal noswapfile noundofile nobackup

    " Switch to binary mode to read the encrypted file
    autocmd BufReadPre,FileReadPre encrypted setlocal bin
    autocmd BufReadPre,FileReadPre encrypted let ch_save = &ch|setlocal ch=2
    " (If you use tcsh, you may need to alter this line.)
    autocmd BufReadPost,FileReadPost encrypted
                \ '[,']!gpg --decrypt 2> /dev/null

    " Switch to normal mode for editing
    autocmd BufReadPost,FileReadPost encrypted setlocal nobin
    autocmd BufReadPost,FileReadPost encrypted
                \ let &ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost encrypted
                \ execute ':doautocmd BufReadPost ' . expand('%:r')

    " Convert all text to encrypted text before writing
    " (If you use tcsh, you may need to alter this line.)
    autocmd BufWritePre,FileWritePre encrypted
                \ '[,']!gpg --default-recipient-self -ae 2>/dev/null
    " Undo the encryption so we are back in the normal text, directly
    " after the file has been written.
    autocmd BufWritePost,FileWritePost encrypted undo
augroup END

autocmd! BufRead,BufNewFile *.asc,*.gpg,*.pgp setfiletype encrypted

