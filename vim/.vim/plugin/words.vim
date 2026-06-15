" Summary: Spell and dictionary related config.
" Help:    N/A

" Regenerate spell files from word lists.
command! -nargs=0 -bar -bang Mkspell
    \ call glob('~/.vim/spell/*', 1, 1)
    \ ->foreach("exec '<mods> mkspell<bang>' v:val")
silent! Mkspell

" Generate a dictionary file from custom spell files.  (Excl. rare words.)
command! -nargs=0 -bar MkDictFromSpell
    \ call glob('~/.vim/spell/*', 1, 1)
    \ ->filter("v:val !~# '\\m.spl$'")
    \ ->map("readfile(v:val)")
    \ ->flatten()
    \ ->filter("v:val !~# '\\m\\(^/\\|/[!?][1-9]\\?$\\)'")
    \ ->map("substitute(v:val, '\\m\\C/[=][1-9]\\?$', '', '')")
    \ ->writefile(expand('~/.vim/state/dict'), '')
