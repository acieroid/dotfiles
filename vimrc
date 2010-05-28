" Look
set vb
set bg=dark
syn on
set number

" Indentation
set cindent
set expandtab
set ai
set ts=2
set sw=2
"autocmd BufRead,BufNewFile *.c set cindent
"autocmd BufRead,BufNewFile *.h set cindent

" Status Line :
set statusline=%F%m%r%h%w\ %{&ff}\ %Y\ hex:0x\%02.2B\ pos:%0l,%0v\ [%p%%] 
set laststatus=2

" aspell with .text files
set spelllang=fr,en
autocmd BufRead,BufNewFile *.tex set spell

" bépo configuration
source ~/.vimrc.bepo
highlight NbSp ctermbg=lightgray guibg=lightred
match NbSp /\%xa0/

" gf edit the file under the cursor, and create it if it doesn't exist
map gf :tabedit <cfile><CR> 

" search
set incsearch
set ignorecase
set hls

" ii instead of Escape
map! ii <Esc>
" F1 is really annoying when pressed accidentally instead of escape
map! <F1> <Esc>

" for slimv
let g:slimv_repl_split = 0
let g:slimv_repl_open = 0
let g:slimv_impl = 'sbcl-rlwrapped'
let g:slimv_client = 'python ~/.vim/ftplugin/slimv.py -l sbcl-rlwrapped' 
let g:slimv_clhs_root = 'file:/usr/share/doc/HyperSpec/'
let g:lisp_rainbow = 1

" Other stuff
au BufRead,BufNewFile *.arc setf arc 
au BufRead,BufNewFile Makefile set noexpandtab
set mouse=
set nocompatible
set ttyfast
