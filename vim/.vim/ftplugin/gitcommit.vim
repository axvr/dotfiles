" =======================================
" File:
"
"
" =======================================

setlocal nofoldenable
setlocal spell


" Committia.vim lightweight replacement

" TODO significantly improve
"   - close when gitcommit closed
"   - start at the top of the buffer in gitcommit
"   - don't allow running twice
"   - try to remove the 'Press Enter to continue.' message
"   - work with other VCS
"   - check if has VCS
"   - work with vim-fugitive 'Gcommit'
"   - and more ...
"   - don't open diff when using fugitive :Gstatus

let gitDiff = system('git diff --cached')

" TODO check not already open in buffer
botright split /tmp/gitdiff
botright put =gitDiff
botright setlocal filetype=diff
botright setlocal nomodifiable
unlet gitDiff

