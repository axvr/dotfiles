au BufRead,BufNewFile * if getline(1) =~# '^#!.*\<csi\>' | let b:is_chicken = 1 | setf scheme | endif
