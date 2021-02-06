com! -nargs=+ -complete=file GitGrep let s:gp=&gp|set gp=git\ grep\ -n|gr <args>|let &gp=s:gp|unl s:gp
com! -nargs=? -range GitBlame ec join(systemlist("git -C ".shellescape(expand('%:p:h')).
            \ " blame -L <line1>,<line2> <args> -- ".expand('%:t')),"\n")
