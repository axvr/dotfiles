" =============================================================
" Description:  Set custom keymaps & commands for Vim
" File:         ~/.vim/plugin/keymappings.vim
" =============================================================

" Spell check toggle
nnoremap <Leader>ss :<C-u>setlocal spell!<CR>
" Make tags file using ctags
command! -nargs=0 MakeTags !ctags -R .
nnoremap <silent> <Leader>mt :<C-u>MakeTags<CR>
" Remove trailing whitespace
command! -nargs=0 -bar Trim :%!wtf -t
" Allow quick changing of termguicolors
nnoremap <Leader>tc :<C-u>set termguicolors!<CR>
" Toggle the Undotree
nnoremap <leader>ut :<C-u>UndotreeToggle<CR>
