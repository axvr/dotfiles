" =============================================================
" Description:  Improve .NET Core Development on Vim
" File:         ~/.vim/ftdetect/dotnet.vim
" =============================================================

autocmd BufNewFile *.cshtml %d|r ~/.vim/skeleton/skeleton.cshtml|1d
autocmd BufNewFile *.cs     %d|r ~/.vim/skeleton/skeleton.cs|1d
autocmd BufNewFile *.csproj %d|r ~/.vim/skeleton/skeleton.csproj|1d
