" Summary: Better searching and search tools.
" Help:    N/A

" Faster `:find` and `:grep`.
function! s:find_fuzzy(cmdarg, _) abort
    return axvr#MatchFuzzy(systemlist('fd -HE .git -d 8 .'), a:cmdarg)
endfunction
if executable('fd') && exists('+findfunc') | set findfunc=s:find_fuzzy | endif
if executable('rg') | set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ -g\ '!.git/*' | endif

nnoremap <leader>/ :silent grep! ''<left>
nnoremap <leader>f :find<space>
nnoremap <leader>b :buffer<space>

" Quickly use alternate grep-style output tools.
"   :GrepWith todos % | grep src
"   :GrepWith git grep -n something
"   :GrepWithAdd todos %
"   :LgrepWith git markers
"   :LgrepaddWith todos %
" TODO: switch to "shellcmd" when "shellcmdline" is not available.
com! -nargs=+ -bang -complete=shellcmdline GrepWith     call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 0, 'loc': 0})
com! -nargs=+ -bang -complete=shellcmdline GrepaddWith  call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 1, 'loc': 0})
com! -nargs=+ -bang -complete=shellcmdline LgrepWith    call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 0, 'loc': 1})
com! -nargs=+ -bang -complete=shellcmdline LgrepaddWith call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 1, 'loc': 1})

com! -nargs=* -bang -complete=file_in_path Todos exec 'GrepWith'..<q-bang> 'todos' <q-args>

" Alternatively use `:cgetexpr` and similar commands.
function! s:GrepWith(prg, args = '', opts = {})
    let jump = get(a:opts, 'jump', 1) ? ''    : '!'
    let add  = get(a:opts, 'add', 0)  ? 'add' : ''
    let loc  = get(a:opts, 'loc', 0)  ? 'l'   : ''
    let grep = loc .. 'grep' .. add .. jump .. ' '
    call axvr#TempSetBufOpt('&grepprg', a:prg, {-> execute(grep .. a:args)})
endfunction

if ! has('nvim')
    augroup axvr/search
        autocmd!
        autocmd QuickFixCmdPost,ShellCmdPost * redraw!
    augroup END
endif
