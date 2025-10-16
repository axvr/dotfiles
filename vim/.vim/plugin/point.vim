" Summary: Point to the word under the cursor.  (Wrapper around ":match".)
" Help:    :help axvr/point

function! s:Point(bang, count, cword) abort
    let match = a:count .. 'match'
    let group = {0: 'MoreMsg', 1: 'MoreMsg', 2: 'PmenuMatch', 3: 'PmenuMatchSel'}[a:count]
    let patrn = '/\m\<' .. axvr#ReEscape(a:cword) .. '\>/'
    exe match .. ' ' .. (a:bang ? 'none' : group .. ' ' .. patrn)
endfunction

command! -count -bar -bang -nargs=0 Point
            \ call s:Point(<q-bang> ==# '!', <f-count>, expand('<cword>'))

command! -bar -nargs=0 NoPoint :Point! <bar> Point!2 <bar> Point!3

amenu PopUp.-Point- :
amenu PopUp.Point\ 1 :Point1<CR>
amenu PopUp.Point\ 2 :Point2<CR>
amenu PopUp.Point\ 3 :Point3<CR>
amenu PopUp.No\ point :NoPoint<CR>
