" Summary: Better searching and search tools.
" Help:    N/A

if executable('git') && filereadable('.git')
    let g:list_files_cmd = 'git ls-files -co --exclude-standard | parents | sort -u'
elseif executable('fd')
    let g:list_files_cmd = 'fd --hidden --exclude .git --color never --max-depth 12 .'
endif

" Faster `:find` and `:grep`.
if executable('rg') | set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ -g\ '!.git/*' | endif
if exists('g:list_files_cmd') && exists('+findfunc')
    function! s:find_fuzzy(cmdarg, _) abort
        return axvr#MatchFuzzy(systemlist(g:list_files_cmd), a:cmdarg)
    endfunction

    set findfunc=s:find_fuzzy
endif

nnoremap <leader>/ :silent grep! ''<left>
nnoremap <leader>f :find<space>
nnoremap <leader>b :buffer<space>

" Based on: <https://github.com/jhawthorn/fzy#use-with-vim>
if ! has('nvim')
    function! Fzy(choice_cmd, callback) abort
        try
            let output = system(a:choice_cmd .. ' | fzy')
            if v:shell_error == 0 && !empty(output)
                call a:callback(output)
            endif
        finally
            redraw!
        endtry
    endfunction
endif

" Interactive file fuzzy finder.
if exists('g:list_files_cmd') && exists('*Fzy')
    nnoremap <leader><space> :call Fzy(g:list_files_cmd, {out -> execute('edit ' .. out)})<CR>
else
    nnoremap <leader><space> :find<space>
endif

" Quickly use alternate grep-style output tools.
"   :GrepWith todos % | grep src
"   :GrepWith git grep -n something
"   :GrepWithAdd todos %
"   :LgrepWith git markers
"   :LgrepaddWith todos %
if v:version > 901 || has('nvim')
    com! -nargs=+ -bang -complete=shellcmdline GrepWith     call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 0, 'loc': 0})
    com! -nargs=+ -bang -complete=shellcmdline GrepaddWith  call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 1, 'loc': 0})
    com! -nargs=+ -bang -complete=shellcmdline LgrepWith    call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 0, 'loc': 1})
    com! -nargs=+ -bang -complete=shellcmdline LgrepaddWith call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 1, 'loc': 1})
else
    " Use "shellcmd" when "shellcmdline" is not available.
    com! -nargs=+ -bang -complete=shellcmd GrepWith     call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 0, 'loc': 0})
    com! -nargs=+ -bang -complete=shellcmd GrepaddWith  call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 1, 'loc': 0})
    com! -nargs=+ -bang -complete=shellcmd LgrepWith    call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 0, 'loc': 1})
    com! -nargs=+ -bang -complete=shellcmd LgrepaddWith call s:GrepWith('exec', <q-args>, {'jump': empty(<q-bang>), 'add': 1, 'loc': 1})
endif

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
