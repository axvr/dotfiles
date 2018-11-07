" =============================================================
" Description:  Set custom keymaps & commands for Vim
" File:         ~/.vim/plugin/keymappings.vim
" =============================================================

" Spell check toggle
nnoremap <Leader>ss :<C-u>setlocal spell!<CR>
" Allow quick changing of termguicolors
nnoremap <Leader>tc :<C-u>set termguicolors!<CR>
" Toggle the Undotree
nnoremap <Leader>ut :<C-u>UndotreeToggle<CR>
" Display additional file information
nnoremap <Leader>fi :<C-u>echo &fenc?&fenc:&enc '' &ff '' &ft<CR>
" Display current VCS branch
nnoremap <Leader>gb :<C-u>echo system('vcs -b')<CR>
