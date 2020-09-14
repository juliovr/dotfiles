set title 	" Show filename in terminal window
set number 	" Show line numbers
set mouse=a " Mouse integration

set t_Co=256
set background=dark
"colorscheme PaperColor

set tabstop=4
set shiftwidth=4
set softtabstop=4
set shiftround
set expandtab 	" Insert spaces for tabs

set ignorecase  " Ignore uppercase in search 
set smartcase  " Only ignore uppercase if the text has no uppercase letters

set encoding=utf-8

set autoread " automatically reload files upon change outside VIM

let g:mapleader = ' '  " Space as leader key
let g:loaded_clipboard_provider = 0
set clipboard+=unnamedplus

" Save using <leader> + s
nnoremap <leader>s :w<CR>

" <líder> + y to copy to the clipboard
vnoremap <leader>c "+y
nnoremap <leader>c "+y

" <líder> + d to cut to the clipboard
vnoremap <leader>x "+d
nnoremap <leader>x "+d

" <líder> + v to paste from the clipboard
nnoremap <leader>v "+p
vnoremap <leader>v "+p

" Next buffer
nnoremap <leader>l :bnext<CR>

" Previous buffer
nnoremap <leader>h :bprevious<CR>

" Close current buffer
nnoremap <leader>q :bdelete<CR>

" Commenting blocks of code.
augroup commenting_blocks_of_code
  autocmd!
  autocmd FileType c,cpp,java,scala,go  let b:comment_leader = '// '
  autocmd FileType sh,ruby,python       let b:comment_leader = '# '
  autocmd FileType conf,fstab           let b:comment_leader = '# '
  autocmd FileType tex                  let b:comment_leader = '% '
  autocmd FileType mail                 let b:comment_leader = '> '
  autocmd FileType vim                  let b:comment_leader = '" '
augroup END
noremap <silent> cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>



" ===============================================
" start GO
" ===============================================
" Turn off warning for nvim version for vim-go
let g:go_version_warning = 0

let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

" Auto format and import
let g:go_fmt_command = "goimports"

" Highlight
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_auto_sameids = 1
" let g:go_highlight_operators = 1

" Show method definitions automatically every 800ms (default)
let g:go_auto_type_info = 1

" Go build and run using leader b and r
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>t  <Plug>(go-test)



" -------------------------------------------------------------------------------------------------
" coc.nvim default settings
" -------------------------------------------------------------------------------------------------

" if hidden is not set, TextEdit might fail.
set hidden
" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use U to show documentation in preview window
nnoremap <silent> U :call <SID>show_documentation()<CR>

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" disable vim-go :GoDef short cut (gd)
" this is handled by LanguageClient [LC]
let g:go_def_mapping_enabled = 0



" ===============================================
" end GO
" ===============================================


" ==============================================
" Plugins vim-plug
" Install plugins using :PlugInstall
" ==============================================
call plug#begin('~/.config/nvim/plugins')

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Initialize plugin system
call plug#end()
