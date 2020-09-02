highlight! def link schemeParentheses Delimiter
syntax keyword schemeTodo TODO FIXME XXX HACK BUG NOTE UNDONE
            \ TODO: FIXME: XXX: HACK: BUG: NOTE: UNDONE:
            \ containedin=schemeComment,schemeMultilineComment
highlight def link schemeTodo Todo
