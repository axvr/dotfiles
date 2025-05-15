" Project management helpers.

function! TempSetBufOpt(opt, val, callback)
    let buf = bufnr('%')
    let prevval = getbufvar(buf, a:opt)
    call setbufvar(buf, a:opt, a:val)
    call a:callback()
    call setbufvar(buf, a:opt, prevval)
endfunction

function! TempGrep(prg, args)
    call TempSetBufOpt('&grepprg', a:prg, {-> execute('grep ' . a:args)})
    redraw!
endfunction

" Task management.
command! -nargs=0 -bar Tasks tabe DONE | split DOING | split TODO
command! -nargs=* -complete=file Todos call TempGrep('todos', <q-args>)

" Git integration.
" TODO: replace Fugitive's :Ggrep with this?
command! -nargs=+ -complete=file GitGrep call TempGrep('git grep -n --column', <q-args>)

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig
            \ <mods> new
            \ | read ++edit # | 0d_ | diffthis
            \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
            \ | exe 'setfiletype ' . getbufvar('#', '&l:filetype')
            \ | exe 'silent file [Diff] ' . bufname('#')
            \ | wincmd p | diffthis

" Prompt loading a ".axvr.vim" file to load extra configs.
" TODO: checksum and ask to trust file on load.
" TODO: create an autoload file containing trust functions.
" confirm('".axvr.vim" file changed.  Load and trust?', "&Yes\n&No", 2, 'Question') == 1
if filereadable('.axvr.vim')
    source .axvr.vim
    echohl WarningMsg
    echomsg 'Loaded additional config from ".axvr.vim"'
    echohl NONE
endif

" LSP config.
" :help lsp.txt
if has('nvim')
    command! -nargs=0 -bar LspReferences call v:lua.vim.lsp.buf.references()
    command! -nargs=0 -bar LspDefinition call v:lua.vim.lsp.buf.definition()
    command! -nargs=0 -bar LspCodeAction call v:lua.vim.lsp.buf.code_action()
    command! -nargs=0 -bar LspOverview   call v:lua.vim.lsp.buf.document_symbol()
    command! -nargs=0 -bar LspHoverDoc   call v:lua.vim.lsp.buf.hover()
    command! -nargs=0 -bar LspImplementation call v:lua.vim.lsp.buf.implementation()
    command! -nargs=? -bar LspRename     call v:lua.vim.lsp.buf.rename(<f-args>)
    command! -nargs=0 -bar LspTypeDef    call v:lua.vim.lsp.buf.type_definition()

    nnoremap <C-.> :<C-u>LspCodeAction<CR>

    anoremenu PopUp.-LSP- :
    anoremenu PopUp.LSP\ references <Cmd>LspReferences<CR>
    anoremenu PopUp.LSP\ definition <Cmd>LspDefinition<CR>
    anoremenu PopUp.LSP\ code\ action <Cmd>LspCodeAction<CR>
    anoremenu PopUp.LSP\ rename <Cmd>LspRename<CR>
endif
