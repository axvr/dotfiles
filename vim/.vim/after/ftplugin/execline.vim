" https://codeberg.org/axvr/execline.vim#accessing-execline-documentation

setlocal keywordprg=:ExeclineDocs

function! s:ExeclineProgs()
    if !exists('b:execline_programs')
        let b:execline_programs = uniq(syntaxcomplete#Complete(0, ''))
    endif

    return b:execline_programs
endfunction

function! s:execline_docs(keyword = '') abort
    let base = 'https://skarnet.org/software/execline/'

    if empty(a:keyword)
        exec 'URLOpen' base
    elseif index(s:ExeclineProgs(), a:keyword) >= 0
        exec 'URLOpen' base .. a:keyword .. '.html'
    else
        exec 'Man' a:keyword
    endif
endfunction

command! -nargs=? -bar -complete=customlist,axvr#CmdSynComplete ExeclineDocs
            \ call s:execline_docs(<f-args>)
