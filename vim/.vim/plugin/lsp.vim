packadd lsp

let g:lspServers = [
            \   {
            \     'filetype': ['clojure'],
            \     'path': 'clojure-lsp'
            \   },
            \   {
            \     'filetype': ['bass'],
            \     'path': 'bass',
            \     'args': ['--lsp']
            \   }
            \ ]

call LspAddServer(lspServers)
