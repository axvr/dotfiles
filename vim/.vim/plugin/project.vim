" Summary: Tools for working on projects.
" Help:    N/A

if filereadable('do/build') | set makeprg=do/build | endif

packadd fugitive
nnoremap <leader>gs :Git<CR>
nnoremap <leader>ga :Git add -p<CR>
nnoremap <leader>gd :Gvdiffsplit<CR>

" Quickly save/update session files.
function! s:mksession(file = '') abort
    let sfile = a:file->axvr#Else(v:this_session)->axvr#Else('Session.vim')->fnamemodify(':.')
    echohl Question
    call inputsave()
    let sfile = input('Save session to: ', sfile)
    call inputrestore()
    if !empty(sfile)
        exec 'mksession!' sfile
        echohl SuccessMsg | echo "\nSession saved!"
    endif
    echohl NONE
endfunction
nnoremap <silent> <leader>S :<C-u>call <SID>mksession()<CR>

" Notes and task management.
command! -nargs=0 -bar Tasks tabedit DONE | split DOING | split TODO
command! -nargs=0 -bar Notes <mods> tabedit $NOTES_DIR | silent lcd $NOTES_DIR | arglocal
" TODO: <leader>n mappings?  Go to root, up dir?

" :help diff-original-file
command! -nargs=0 -bar DiffOrig
    \ <mods> new
    \ | read ++edit # | 0d_ | diffthis
    \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
    \ | exe 'setfiletype ' .. getbufvar('#', '&l:filetype')
    \ | exe 'silent file [Diff] ' .. bufname('#')
    \ | wincmd p | diffthis

" Neovim LSP and diagnostic config.
if ! has('nvim') | finish | endif

" :help lsp.txt  (Default keymaps -> :help lsp-defaults)
command! -nargs=0 -bar LspReferences call v:lua.vim.lsp.buf.references()
command! -nargs=0 -bar LspDefinition call v:lua.vim.lsp.buf.definition()
command! -nargs=0 -bar LspCodeAction call v:lua.vim.lsp.buf.code_action()
command! -nargs=0 -bar LspOverview   call v:lua.vim.lsp.buf.document_symbol()
command! -nargs=0 -bar LspHoverDoc   call v:lua.vim.lsp.buf.hover()
command! -nargs=0 -bar LspImplementation call v:lua.vim.lsp.buf.implementation()
command! -nargs=? -bar LspRename     call v:lua.vim.lsp.buf.rename(<f-args>)
command! -nargs=0 -bar LspTypeDef    call v:lua.vim.lsp.buf.type_definition()
command! -nargs=0 -bar LspTerminate  call v:lua.vim.lsp.stop_client(v:lua.vim.lsp.get_clients())

" :help diagnostic.txt  (Default keymaps -> :help diagnostic-defaults)
command! -nargs=0 -bar DiagExplain   call v:lua.vim.diagnostic.open_float({"scope": "line"})
command! -nargs=0 -bar DiagToQfList  call v:lua.vim.diagnostic.setqflist()
command! -nargs=0 -bar DiagToLocList call v:lua.vim.diagnostic.setloclist()

" TODO: switch to Elixir Expert LSP once ready.
call v:lua.vim.lsp.enable('elixirls')
