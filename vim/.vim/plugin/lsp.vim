" Summary: Neovim LSP and diagnostic config.
" Help:    :help lsp.txt  |  :help diagnostic.txt

if has('nvim')
    " Default keymaps -> :help lsp-defaults
    command! -nargs=0 -bar LspReferences call v:lua.vim.lsp.buf.references()
    command! -nargs=0 -bar LspDefinition call v:lua.vim.lsp.buf.definition()
    command! -nargs=0 -bar LspCodeAction call v:lua.vim.lsp.buf.code_action()
    command! -nargs=0 -bar LspOverview   call v:lua.vim.lsp.buf.document_symbol()
    command! -nargs=0 -bar LspHoverDoc   call v:lua.vim.lsp.buf.hover()
    command! -nargs=0 -bar LspImplementation call v:lua.vim.lsp.buf.implementation()
    command! -nargs=? -bar LspRename     call v:lua.vim.lsp.buf.rename(<f-args>)
    command! -nargs=0 -bar LspTypeDef    call v:lua.vim.lsp.buf.type_definition()
    command! -nargs=0 -bar LspKillAll    call v:lua.vim.lsp.stop_client(v:lua.vim.lsp.get_clients())

    " Default keymaps -> :help diagnostic-defaults
    command! -nargs=0 -bar DiagExplain   call v:lua.vim.diagnostic.open_float({"scope": "line"})
    command! -nargs=0 -bar DiagToQfList  call v:lua.vim.diagnostic.setqflist()
    command! -nargs=0 -bar DiagToLocList call v:lua.vim.diagnostic.setloclist()

    call v:lua.vim.lsp.enable('elixirls')
endif
