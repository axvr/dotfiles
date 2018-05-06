" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

" TODO create better mappings (using leader and localleader)
" TODO add maps to start and stop server like in Spacemacs
" TODO delete useless omnisharp commands
" TODO use File to fully remove vim-csharp plugin
" TODO add scaffolding interface?
setlocal commentstring=//%s updatetime=1000
setlocal completeopt& completeopt-=preview splitbelow&
nnoremap <buffer><silent> K    :<C-u>OmniSharpDocumentation<CR>:wincmd k<CR>
nnoremap <buffer><silent> gg=G :<C-u>OmniSharpCodeFormat<CR>G
nnoremap <buffer><silent> <leader>. :<C-u>OmniSharpGetCodeActions<CR>
autocmd! CursorHold *.cs :OmniSharpTypeLookup
