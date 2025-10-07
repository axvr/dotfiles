" Argbook: Arglists as file bookmarks.  Save/restore/edit/navigate arglists.

" Argbook, Barge, Bargain?

" - Something like <https://github.com/idbrii/vim-argedit/blob/master/doc/argedit.txt>
" - Press enter to open file.
" - Shortcut to bookmark a file?
" - Command/binding to jump back to current arglist entry.
" - Save/restore arglists.
" - Dirvish has arglist integration.
" - Like harpoon but simpler
" - Write as normal file and it'll update the arglist.
" - Render as dirvish file?

function! ReadArgbook() abort
    normal "_%d
    call argv()->map({_, v -> fnamemodify(v, ':p:.')})
              \->map({i, l -> setbufline(bufnr('%'), i+1, l)})
    setlocal nomodified
endfunction

" TODO: keep currently selected arg file?
function! WriteArgbook() abort
    %argdelete
    for arg in getbufline(bufnr('%'), 1, '$')
        if !empty(arg)
            exec '$argadd' fnameescape(fnamemodify(arg, ':.:p'))
        endif
    endfor
    setlocal nomodified
endfunction

function! ReloadArgbook() abort
    if !&l:modified
        exec 'silent doau BufReadCmd' bufnr('%')
    endif
endfunction

let s:arglistid_to_bufnr = {}

function! OpenArgbook() abort
    let arglistid = arglistid()
    let bufnr = get(s:arglistid_to_bufnr, arglistid, 0)
    if bufnr
        exec 'buffer' bufnr
    else
        enew
        let s:arglistid_to_bufnr[arglistid] = bufnr('%')
        " TODO: move to custom ftplugin.
        " TODO: buffer not hidden!
        setl buftype=acwrite bufhidden=hide
        exec 'file [Arglist: ' . arglistid . ']'
        augroup argbook
            autocmd! BufWriteCmd <buffer> call WriteArgbook()
            autocmd! BufReadCmd  <buffer> call ReadArgbook()
            " autocmd! BufEnter    <buffer> call ReloadArgbook()
            " TODO: update after manually running arglist commands.
        augroup END
        edit
    endif
endfunction

nnoremap _ :call OpenArgbook()<CR>

augroup argbook
    autocmd!
    if has('nvim')
        autocmd TabNewEntered * arglocal | %argdelete
    endif
augroup END

" Global mappings and commands:
" _            | View/edit local arglist.
" gA           | Jump to the current arglist file.
" gX           | Add file to local arglist.
" :Barge       | Same as _

" Maybe commands:  (prob not needed as you could just `:w ...` and `:r ...`)
" :BargeSave n | Save local arglist to named file.
" :BargeLoad n | Load local arglist from named file.

" Barge file mappings:
" <CR>         | Change arglist index to this file and/or open this file.
" p            | Preview file like in Dirvish?
" :w           | Apply arglist changes.
