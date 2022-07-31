vim9script

def Format(lines: list<string>): string
    return join(lines, "\<CR>") .. "\<CR>"
enddef

b:repl_config = {
      'cmd': 'bass',
      'rlwrap': 1,
      'formatter': Format
    }
