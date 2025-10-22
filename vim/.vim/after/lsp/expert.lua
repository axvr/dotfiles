vim.lsp.config['expert'] = {
    cmd = { 'expert' },
    filetypes = { 'elixir', 'eelixir', 'heex', 'surface' },
    -- Taken from: https://github.com/neovim/nvim-lspconfig/blob/e688b486fe9291f151eae7e5c0b5a5c4ef980847/lsp/elixirls.lua#L36-L45
    root_dir = function(bufnr, on_dir)
        local fname = vim.api.nvim_buf_get_name(bufnr)
        -- Elixir workspaces may have multiple `mix.exs` files, for an "umbrella" layout or monorepo.
        -- So we specify `limit=2` and treat the highest one (if any) as the root of an umbrella app.
        local matches = vim.fs.find({ 'mix.exs' }, { upward = true, limit = 2, path = fname })
        local child_or_root_path, maybe_umbrella_path = unpack(matches)
        local root_dir = vim.fs.dirname(maybe_umbrella_path or child_or_root_path)
        on_dir(root_dir)
    end,
    settings = { }
}
