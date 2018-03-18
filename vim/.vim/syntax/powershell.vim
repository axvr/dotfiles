" =============================================================
" Description:  Vim PowerShell Syntax File
" File:         ~/.vim/ftplugin/powershell.vim
" Author:       Peter Provost <peter@provost.org>
" Licence:      Apache Licence 2.0  2005-2012  Peter Provost
" Repository:   https://github.com/PProvost/vim-ps1
" =============================================================

if exists("b:current_syntax")
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

" Sync-ing method
syn sync minlines=100

" Operators contain dashes
setlocal iskeyword+=-
" PowerShell doesn't care about case
syn case ignore

" Certain tokens can't appear at the top level of the document
syn cluster ps1NotTop contains=@ps1Comment,ps1CDocParam,ps1FunctionDeclaration

" Comments and special comment words
syn keyword ps1CommentTodo TODO FIXME XXX NOTE contained
syn match ps1CDocParam /.*/ contained
syn match ps1CommentDoc /^\s*\zs\.\w\+\>/ nextgroup=ps1CDocParam contained
syn match ps1CommentDoc /#\s*\zs\.\w\+\>/ nextgroup=ps1CDocParam contained
syn match ps1Comment /#.*/ contains=ps1CommentTodo,ps1CommentDoc,@Spell
syn region ps1Comment start="<#" end="#>" contains=ps1CommentTodo,ps1CommentDoc,@Spell

" Language keywords and elements
syn keyword ps1Conditional if else elseif switch default
syn keyword ps1Repeat while for do until break continue foreach in
syn match ps1Repeat  /\<foreach\>/ nextgroup=ps1Block skipwhite
syn match ps1Keyword /\<while\>/   nextgroup=ps1Block skipwhite
syn match ps1Keyword /\<where\>/   nextgroup=ps1Block skipwhite

syn keyword ps1Exception begin process end exit inlinescript parallel sequence
syn keyword ps1Keyword   try catch finally throw
syn keyword ps1Keyword   return filter in trap param data dynamicparam
syn keyword ps1Constant  $true $false $null
syn match   ps1Constant  +\$?+
syn match   ps1Constant  +\$_+
syn match   ps1Constant  +\$\$+
syn match   ps1Constant  +\$^+

" Keywords reserved for future use
syn keyword ps1Keyword class define from using var

" Function declarations
syn keyword ps1Keyword function      nextgroup=ps1FunctionDeclaration skipwhite
syn keyword ps1Keyword filter        nextgroup=ps1FunctionDeclaration skipwhite
syn keyword ps1Keyword workflow      nextgroup=ps1FunctionDeclaration skipwhite
syn keyword ps1Keyword configuration nextgroup=ps1FunctionDeclaration skipwhite
syn keyword ps1Keyword class         nextgroup=ps1FunctionDeclaration skipwhite
syn keyword ps1Keyword enum          nextgroup=ps1FunctionDeclaration skipwhite
syn match   ps1FunctionDeclaration   /\w\+\(-\w\+\)*/ contained

" Function invocations
syn match ps1FunctionInvocation /\w\+\(-\w\+\)\+/

" Type declarations
syn match ps1Type /\[[a-z_][a-z0-9_.,\[\]]\+\]/

" Variable references
syn match ps1ScopeModifier /\(global:\|local:\|private:\|script:\)/ contained
syn match ps1Variable      /\$\w\+\(:\w\+\)\?/                      contains=ps1ScopeModifier
syn match ps1Variable      /\${\w\+\(:\w\+\)\?}/                    contains=ps1ScopeModifier

" Operators
syn keyword ps1Operator -eq -ne -ge -gt -lt -le -like -notlike -match -notmatch -replace -split -contains -notcontains
syn keyword ps1Operator -ieq -ine -ige -igt -ile -ilt -ilike -inotlike -imatch -inotmatch -ireplace -isplit -icontains -inotcontains
syn keyword ps1Operator -ceq -cne -cge -cgt -clt -cle -clike -cnotlike -cmatch -cnotmatch -creplace -csplit -ccontains -cnotcontains
syn keyword ps1Operator -in -notin
syn keyword ps1Operator -is -isnot -as -join
syn keyword ps1Operator -and -or -not -xor -band -bor -bnot -bxor
syn keyword ps1Operator -f
syn match ps1Operator /!/
syn match ps1Operator /=/
syn match ps1Operator /+=/
syn match ps1Operator /-=/
syn match ps1Operator /\*=/
syn match ps1Operator /\/=/
syn match ps1Operator /%=/
syn match ps1Operator /+/
syn match ps1Operator /-\(\s\|\d\|\.\|\$\|(\)\@=/
syn match ps1Operator /\*/
syn match ps1Operator /\//
syn match ps1Operator /|/
syn match ps1Operator /%/
syn match ps1Operator /&/
syn match ps1Operator /::/
syn match ps1Operator /,/
syn match ps1Operator /\(^\|\s\)\@<=\. \@=/

" Regular Strings
" These aren't precisely correct and could use some work
syn region ps1String start=/"/ skip=/`"/ end=/"/ contains=@ps1StringSpecial,@Spell
syn region ps1String start=/'/ skip=/''/ end=/'/

" Here-Strings
syn region ps1String start=/@"$/ end=/^"@/ contains=@ps1StringSpecial,@Spell
syn region ps1String start=/@'$/ end=/^'@/

" Interpolation
syn match   ps1Escape /`./
syn region  ps1Interpolation matchgroup=ps1InterpolationDelimiter start="$(" end=")" contained contains=ALLBUT,@ps1NotTop
syn region  ps1NestedParentheses start="(" skip="\\\\\|\\)" matchgroup=ps1Interpolation end=")" transparent contained
syn cluster ps1StringSpecial contains=ps1Escape,ps1Interpolation,ps1Variable,ps1Boolean,ps1Constant,ps1BuiltIn,@Spell

" Numbers
syn match ps1Number "\(\<\|-\)\@<=\(0[xX]\x\+\|\d\+\)\([KMGTP][B]\)\=\(\>\|-\)\@="
syn match ps1Number "\(\(\<\|-\)\@<=\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[dD]\="
syn match ps1Number "\<\d\+[eE][-+]\=\d\+[dD]\=\>"
syn match ps1Number "\<\d\+\([eE][-+]\=\d\+\)\=[dD]\>"

" Constants
syn match ps1Boolean  "$\%(true\|false\)\>"
syn match ps1Constant /\$null\>/
syn match ps1BuiltIn  "$^\|$?\|$_\|$\$"
syn match ps1BuiltIn  "$\%(args\|error\|foreach\|home\|input\)\>"
syn match ps1BuiltIn  "$\%(match\(es\)\?\|myinvocation\|host\|lastexitcode\)\>"
syn match ps1BuiltIn  "$\%(ofs\|shellid\|stacktrace\)\>"

" Blocks
syn region ps1Block start=/{/ end=/}/ transparent
syn region ps1Region start=/#region/ end=/#endregion/ transparent fold keepend extend

" Setup default color highlighting
hi def link ps1Number                 Number
hi def link ps1Block                  Block
hi def link ps1Region                 Region
hi def link ps1Exception              Exception
hi def link ps1Constant               Constant
hi def link ps1String                 String
hi def link ps1Escape                 SpecialChar
hi def link ps1InterpolationDelimiter Delimiter
hi def link ps1Conditional            Conditional
hi def link ps1FunctionDeclaration    Function
hi def link ps1FunctionInvocation     Function
hi def link ps1Variable               Identifier
hi def link ps1Boolean                Boolean
hi def link ps1BuiltIn                StorageClass
hi def link ps1Type                   Type
hi def link ps1ScopeModifier          StorageClass
hi def link ps1Comment                Comment
hi def link ps1CommentTodo            Todo
hi def link ps1CommentDoc             Tag
hi def link ps1CDocParam              Todo
hi def link ps1Operator               Operator
hi def link ps1Repeat                 Repeat
hi def link ps1Keyword                Keyword

let b:current_syntax = "ps1"

let &cpo = s:cpo_save
unlet s:cpo_save
