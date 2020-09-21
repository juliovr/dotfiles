set title       " Show filename in terminal window
set number      " Show current line number
set relativenumber " Show relative line number
set tabstop=4
set shiftwidth=4
set softtabstop=4
set shiftround
set expandtab   " Insert spaces for tabs
set ignorecase  " Ignore uppercase in search 
set smartcase  " Only ignore uppercase if the text has no uppercase letters
set encoding=utf-8
set autoread " automatically reload files upon change outside VIM
set mouse=a
set clipboard+=unnamedplus
set cursorline

let g:mapleader = ' '  " Space as leader key

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

