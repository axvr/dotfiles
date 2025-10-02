-- TODO: switch to Elixir Expert LSP once ready.

vim.lsp.config['elixirls'] = {
    cmd = { vim.env.HOME .. '/.local/share/mise/installs/elixir-ls/latest/language_server.sh' },
    filetypes = { 'elixir' },
    root_markers = { '.git', 'mix.lock', 'mix.exs' },
    settings = {
        elixirLS = {
            dialyzerEnabled = true,
            enableTestLenses = false,
            fetchDeps = false,
            suggestSpecs = false
        }
    }
}
