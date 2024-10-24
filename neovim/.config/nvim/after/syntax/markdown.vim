function! s:GetSynCmd(name)
    return execute('syn list ' . a:name)->split("\n")[1]
                \->trim()->substitute('\m\C^' . a:name . '\s*xxx\s*', '', '')
endfunction

" Extend Markdown concealing to other syntax elements.
call execute('syn region markdownLink ' . s:GetSynCmd('markdownLink') . ' conceal')
call execute('syn region markdownId ' . s:GetSynCmd('markdownId') . ' conceal')
call execute('syn region markdownLinkText ' . s:GetSynCmd('markdownLinkText') . ' concealends')

" Set colour of text within Markdown code blocks.
hi! link markdownCodeBlock Function
hi! link markdownCode markdownCodeBlock

" Basic syntax highlighting for GitHub Markdown Alerts:
" https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#alerts
syntax match markdownGitHubAlertNote /\m\[!NOTE\]/
syntax match markdownGitHubAlertTip  /\m\[!TIP\]/
syntax match markdownGitHubAlertImportant /\m\[!IMPORTANT\]/
syntax match markdownGitHubAlertWarning /\m\[!WARNING\]/
syntax match markdownGitHubAlertCaution /\m\[!CAUTION\]/
hi def link markdownGitHubAlertTip DiagnosticOk
hi def link markdownGitHubAlertNote DiagnosticHint
hi def link markdownGitHubAlertImportant DiagnosticInfo
hi def link markdownGitHubAlertWarning DiagnosticWarn
hi def link markdownGitHubAlertCaution DiagnosticError
