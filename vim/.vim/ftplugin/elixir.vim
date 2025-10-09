setlocal signcolumn=yes

" I still find Elixir code hard to read, so make declarations high contrast.
hi link elixirFunctionDeclaration MatchParen
hi link elixirModuleDeclaration MatchParen
hi link elixirMacroDeclaration MatchParen
hi link elixirExceptionDefine Error
hi link elixirStructDefine Warning
