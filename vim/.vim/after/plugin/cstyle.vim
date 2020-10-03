function Cstyle(style) dict
    if a:style ==# 'gnu'
        call s:gnu()
    elseif a:style ==# 'vim'
        call s:vim()
    else
        echohl ErrorMsg
        echom "C style '" . a:style . "' is not supported."
        echohl None
    endif
endfunction

let g:ascribe_handlers['ctyle'] = function('Cstyle')

" TODO: More C styles:
"   - Kernel
"   - <https://git.sr.ht/~sircmpwn/cstyle>
"   - K&R

" GNU coding standard format.
" <https://www.gnu.org/prep/standards/standards.html>
function! s:gnu()
    setlocal expandtab softtabstop=2 shiftwidth=2 tabstop=8 textwidth=79
    setlocal cindent cinoptions=>2s,es,n-s,f0,{s,}0,^-s,p5,t0,Ls,:s,=s,l1,b0
    setlocal cinoptions+=+2,c3,C0,/0,(0,u4,U0,w0,W0,k0,m0,M0,)20,*70,P0
    setlocal cinoptions+=gs,hs,N0,E0,is,j0,J0,#0  " C++, Java, JS, Perl, etc.
    setlocal formatprg=indent\ 2>/dev/null
endfunction

" Vim defaults.
function! s:vim()
    setlocal noexpandtab softtabstop=8 shiftwidth=8 tabstop=8 textwidth<
    setlocal cindent cinoptions< formatprg<
endfunction
