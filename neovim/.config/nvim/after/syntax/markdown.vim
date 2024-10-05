" Basic syntax highlighting for GitHub Markdown Alerts:
" https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#alerts

syntax match markdownGitHubAlertNote /\[!NOTE\]/
syntax match markdownGitHubAlertTip  /\[!TIP\]/
syntax match markdownGitHubAlertImportant /\[!IMPORTANT\]/
syntax match markdownGitHubAlertWarning /\[!WARNING\]/
syntax match markdownGitHubAlertCaution /\[!CAUTION\]/

hi def link markdownGitHubAlertTip DiagnosticOk
hi def link markdownGitHubAlertNote DiagnosticHint
hi def link markdownGitHubAlertImportant DiagnosticInfo
hi def link markdownGitHubAlertWarning DiagnosticWarn
hi def link markdownGitHubAlertCaution DiagnosticError
