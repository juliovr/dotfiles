" ==============================================
" Plugins vim-plug
" Install plugins using :PlugInstall
" ==============================================
call plug#begin('~/.config/nvim/plugins')

Plug 'joshdick/onedark.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

