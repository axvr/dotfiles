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
