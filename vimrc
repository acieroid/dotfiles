" Look
set vb
set bg=dark
syn on
set number

" Indentation
set expandtab
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

" Other stuff
au BufRead,BufNewFile *.arc setf arc 
au BufRead,BufNewFile Makefile set noexpandtab
set mouse=
set nocompatible
set ttyfast
