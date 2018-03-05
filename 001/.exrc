if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <Nul> 
nnoremap + 
nnoremap - 
nnoremap Y y$
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())
cnoremap  <Down>
cnoremap  <Up>
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set background=dark
set backspace=indent,eol,start
set backup
set backupdir=~/.vim/bak
set clipboard=autoselect
set display=lastline
set expandtab
set fileencodings=utf-8,sjis,iso-2022-jp,euc-jp,ucs-bom,latin1
set guicursor=n-v-c:block,o:hor50,i-ci:hor15,r-cr:hor30,sm:block,a:blinkon0,a:blinkon0
set helplang=ja
set history=200
set hlsearch
set ignorecase
set incsearch
set listchars=tab:>\ ,trail:.
set matchtime=1
set mouse=n
set pumheight=10
set ruler
set runtimepath=~/.vim,/usr/share/vim/vimfiles,/usr/share/vim/vim80,/usr/share/vim/vimfiles/after,~/.vim/after,~/.vim/eclim,~/.vim/eclim/after
set shellpipe=2>&1|\ tee
set shellredir=>%s\ 2>&1
set shiftwidth=4
set showmatch
set smartcase
set softtabstop=4
set tabstop=4
set undodir=./.vimundo,~/.vim/vimundo
set undofile
set viminfo='20,\"50
set wildmenu
set wildmode=list:full
set nowrapscan
" vim: set ft=vim :
