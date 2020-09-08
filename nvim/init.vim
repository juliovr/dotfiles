set title 	" Muestra el nombre del archivo en la ventana de la terminal
set number 	" Muestra los números de las líneas
set mouse=a " Mouse integration

" Indentación a 2 espacios
set tabstop=4
set shiftwidth=4
set softtabstop=4
set shiftround
set expandtab 	" Insertar espacios en lugar de <Tab>s

set ignorecase  " Ignorar mayúsculas al hacer una búsqueda
set smartcase  " No ignorar mayúsculas si la palabra a buscar contiene mayúsculas

set encoding=utf-8

set autoread " automatically reload files upon change outside VIM

let g:mapleader = ' '  " Definir espacio como la tecla líder
nnoremap <leader>s :w<CR>  " Guardar con <líder> + s

" Usar <líder> + y para copiar al portapapeles
vnoremap <leader>c "+y
nnoremap <leader>c "+y

" Usar <líder> + d para cortar al portapapeles
vnoremap <leader>x "+d
nnoremap <leader>x "+d

" Usar <líder> + v para pegar desde el portapapeles
nnoremap <leader>v "+p
vnoremap <leader>v "+p

" Moverse al buffer siguiente con <líder> + l
nnoremap <leader>l :bnext<CR>

" Moverse al buffer anterior con <líder> + j
nnoremap <leader>h :bprevious<CR>

" Cerrar el buffer actual con <líder> + q
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

" ===============================================
" end GO
" ===============================================


" ==============================================
" Plugins
" ==============================================
call plug#begin('~/.config/nvim/plugins')

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

" Initialize plugin system
call plug#end()
