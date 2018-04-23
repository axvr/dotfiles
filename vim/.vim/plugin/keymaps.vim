" =============================================================
" Description:  Set custom keymaps for Vim
" File:         ~/.vim/plugin/keymappings.vim
" =============================================================

" Spell check toggle
nnoremap <Leader>tS :<C-u>setlocal spell!<CR>
" Make tags file using ctags
command! -nargs=0 MakeTags !ctags -R .
nnoremap <silent> <Leader>mt :<C-u>MakeTags<CR>
" Allow quick changing of termguicolors
nnoremap <Leader>tG :<C-u>set termguicolors!<CR>

