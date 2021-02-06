" Description:  Functions to set C formatting style.
" File:         autoload/cstyle.vim

" TODO: add more styles:
"   - K&R
"   - Kernel <https://www.kernel.org/doc/html/v4.10/process/coding-style.html>

" GNU coding standard format.
" <https://www.gnu.org/prep/standards/standards.html>
function! cstyle#gnu()
    setlocal expandtab softtabstop=2 shiftwidth=2 tabstop=8 textwidth=79
    setlocal cindent cinoptions=>2s,es,n-s,f0,{s,}0,^-s,p5,t0,Ls,:s,=s,l1,b0
    setlocal cinoptions+=+2,c3,C0,/0,(0,u4,U0,w0,W0,k0,m0,M0,)20,*70,P0
    setlocal cinoptions+=gs,hs,N0,E0,is,j0,J0,#0  " C++, Java, JS, Perl, etc.
    setlocal formatprg=indent\ 2>/dev/null
endfunction

" Vim defaults.
function! cstyle#vim()
    setlocal noexpandtab softtabstop=8 shiftwidth=8 tabstop=8 textwidth&
    setlocal cindent cinoptions& formatprg&
endfunction
