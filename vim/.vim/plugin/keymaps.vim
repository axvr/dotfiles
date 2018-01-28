" =============================================================
" Description:  Set custom keymaps for Vim
" File:         ~/.vim/plugin/keymappings.vim
" =============================================================

" Set Keymaps & Commands
let mapleader = "\<Space>"
let maplocalleader = ","
" Spell check toggle
nnoremap <Leader>ss :<C-u>setlocal spell!<CR>
" Make tags file using ctags
command! -nargs=0 MakeTags !ctags -R .
nnoremap <silent> <Leader>mt :<C-u>MakeTags<CR>
" Allow quick changing of termguicolors
nnoremap <Leader>tc :<C-u>set termguicolors!<CR>
" Quick file navigation
nnoremap <Leader>ff :<C-u>find<Space>
nnoremap <Leader>fs :<C-u>write<CR>
nnoremap <Leader>bb :<C-u>buffers<CR>:<C-u>buffer<Space>

