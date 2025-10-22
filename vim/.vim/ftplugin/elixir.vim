setlocal signcolumn=yes
compiler mix

" I still find Elixir code hard to read, so make declarations high contrast.
hi link elixirModuleDeclaration IncSearch
hi link elixirMacroDeclaration IncSearch
hi link elixirFunctionDeclaration IncSearch
hi link elixirPrivateFunctionDeclaration Substitute
hi link elixirExceptionDefine Error
hi link elixirStructDefine Warning
