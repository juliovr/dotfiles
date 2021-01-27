set title       " Show filename in terminal window
set number      " Show current line number
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
set guicursor=n-i-v-c:block-nCursor

let g:mapleader = ' '  " Space as leader key

" Map Ctrl-Backspace to delete the previous word in insert mode.
imap <C-BS> <C-W>

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

nnoremap <C-TAB> :b#<CR>

" Next buffer
nnoremap <leader>l :bnext<CR>
nnoremap <C-l> :bnext<CR>

" Previous buffer
nnoremap <leader>h :bprevious<CR>
nnoremap <C-h> :bprevious<CR>

" Close current buffer
nnoremap <leader>q :bdelete<CR>

nnoremap <leader>. :e 

" Indent with TAB and SHIFT-TAB
vnoremap <TAB> >gv
vnoremap <S-TAB> <gv

" Autoclose
inoremap { {}<left>
inoremap <expr> } strpart(getline('.'), col('.')-1, 1) == "}" ? "\<Right>" : "}"

inoremap {<CR> {<CR>}<ESC>O


augroup commenting_blocks_of_code
  autocmd!
  autocmd FileType c,cpp,java,scala,go  let b:comment_leader = '// '
  autocmd FileType sh,ruby,python       let b:comment_leader = '# '
  autocmd FileType conf,fstab           let b:comment_leader = '# '
  autocmd FileType tex                  let b:comment_leader = '% '
  autocmd FileType mail                 let b:comment_leader = '> '
  autocmd FileType vim                  let b:comment_leader = '" '
augroup END
function! Docomment ()
  "make comments on all the lines we've grabbed
  execute '''<,''>s/^\s*/&'.escape(b:comment_leader, '\/').' /e'
endfunction
function! Uncomment ()
  "uncomment on all our lines
  execute '''<,''>s/\v(^\s*)'.escape(b:comment_leader, '\/').'\v\s*/\1/e'
endfunction
function! Comment ()
  "does the first line begin with a comment?
  let l:line=getpos("'<")[1]
  "if there's a match
  if match(getline(l:line), '^\s*'.b:comment_leader)>-1
    call Uncomment()
  else
    call Docomment()
  endif
endfunction
vnoremap <silent> cc :<C-u>call Comment()<cr><cr>
