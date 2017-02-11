filetype plugin on
syntax on
set number
set relativenumber
set mouse=a
set clipboard=unnamedplus
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
set expandtab
set shiftwidth=2
set softtabstop=2
set autoindent
cmap w!! w !sudo tee > /dev/null %
