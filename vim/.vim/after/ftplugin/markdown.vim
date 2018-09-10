" =============================================================
" Description:  Improve Vim's Markdown Editing
" File:         ~/.vim/after/ftplugin/markdown.vim
" =============================================================

setlocal commentstring=<!--%s-->

" Modified version of the default `MarkdownFold()` function, to not react to `#`
" characters in code blocks.
function! MarkdownFold()
  let line = getline(v:lnum)

  " TODO optimise this (rewrite the entire logic in this function)
  if synIDattr(synID(v:lnum, 1, 1), 'name') !=# 'markdownHeadingDelimiter'
      return "="
  endif

  " Regular headers
  let depth = match(line, '\(^#\+\)\@<=\( .*$\)\@=')
  if depth > 0
    return ">" . depth
  endif

  " Setext style headings
  let nextline = getline(v:lnum + 1)
  if (line =~ '^.\+$') && (nextline =~ '^=\+$')
    return ">1"
  endif

  if (line =~ '^.\+$') && (nextline =~ '^-\+$')
    return ">2"
  endif

  return "="
endfunction

setlocal foldexpr=MarkdownFold()
setlocal foldmethod=expr
setlocal foldlevel=1

" Don't enable folding by default, as it can make the file difficult to read.
setlocal nofoldenable
