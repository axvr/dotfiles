" =============================================================
" Description:  Improve .NET Core Development on Vim
" File:         ~/.vim/ftdetect/dotnet.vim
" =============================================================

autocmd BufNewFile *.cs     %d|r ~/.vim/skeleton/skel.cs|1d
autocmd BufNewFile *.csproj %d|r ~/.vim/skeleton/skel.csproj|1d
