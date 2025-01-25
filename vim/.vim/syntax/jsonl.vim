if exists("b:current_syntax")
    finish
endif

runtime syntax/json.vim

syntax clear jsonMissingCommaError

" This block was copied and modified from: https://github.com/kyoh86/vim-jsonl
syntax match jsonMissingCommaError /\("\|\]\|\d\)\zs\_s\+\ze"/
syntax match jsonMissingCommaError /\(\]\|\}\)\zs\_s\+\ze"/      " arrays/objects as values
syntax match jsonMissingCommaError /\(true\|false\)\zs\_s\+\ze"/ " true/false as value

syntax region jsonFold matchgroup=jsonBraces start="{"  end=/}/ transparent fold
syntax region jsonFold matchgroup=jsonBraces start="\[" end=/]/ transparent fold

let b:current_syntax = "jsonl"
