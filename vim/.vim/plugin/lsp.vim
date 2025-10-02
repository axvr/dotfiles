" Neovim LSP config.  (:help lsp.txt)
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

    call v:lua.vim.lsp.enable('elixirls')
endif
