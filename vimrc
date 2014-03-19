" who would want to stay in the past?
set nocompatible
set ttyfast

" look
set vb
set bg=dark
syn on
set number

" indentation, follow the Linux coding style
set noexpandtab
set tabstop=8
set shiftwidth=8
set softtabstop=8
set listchars=tab:⇥␣
set list!
set backspace=2

" Status Line :
set statusline=%F%m%r%h%w\ %{&ff}\ %Y\ hex:0x\%02.2B\ pos:%0l,%0v\ [%p%%]
set laststatus=2

" bépo configuration
source ~/.vimrc.bepo

" highlight non-breakable space characters (because you don't want them in a
" file edited by vim, never)
highlight NbSp ctermbg=lightgray guibg=lightred
match NbSp /\%xa0/

" highlight extra whitespace because we really don't want them anywhere (note
" that setting c_space_errors to 1 will also work, but will have the annoying
" feature of displaying the error when you are currently typing a space at the
" end of the line, and you obviously don't want that).
highlight ExtraWhitespace ctermbg=darkred guibg=lightred
match ExtraWhitespace /\s\+\%#\@<!$/

" remind me that the 81st column is a bitch
set colorcolumn=81

" gf edit the file under the cursor, and create it if it doesn't exist
map gf :tabedit <cfile><CR> 

" search
set incsearch
set ignorecase
set hls

" ii instead of Escape (yes I'm serious)
map! ii <Esc>

" F1 is really annoying when pressed accidentally instead of escape
map! <F1> <Esc>

" useful toggles
map <F1> :set number!<CR>
map <F2> :set expandtab!<CR>
map <F3> :set list!<CR>

" file-dependent stuff
au BufRead,BufNewFile Makefile set noexpandtab
