vim9script

# CustomList function for :command-complete to complete syntax keywords.
export def CmdComplete(text: string, wholecmd: string, curpos: number): list<string>
    const symbols = uniq(syntaxcomplete#Complete(0, ''))
    return empty(text) ? symbols : matchfuzzy(symbols, text)
enddef
