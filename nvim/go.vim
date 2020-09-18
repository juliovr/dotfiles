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

" disable vim-go :GoDef short cut (gd)
" this is handled by LanguageClient [LC]
let g:go_def_mapping_enabled = 0

