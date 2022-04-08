vim9script

# CustomList function for :command-complete to complete syntax keywords.
export def CmdComplete(text: string, wholecmd: string, curpos: number): list<string>
    const symbols = uniq(syntaxcomplete#Complete(0, ''))
    if empty(text)
        return symbols
    else
        return matchfuzzy(symbols, text)
    endif
enddef
