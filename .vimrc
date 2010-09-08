
" General settings 
"===========================================================================================================
set nocompatible
syntax enable
filetype on 
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab
filetype indent on
set number " show line numbers
set hlsearch " highlight search
set incsearch " incremental search
set ignorecase
set smartcase
set infercase " better case handling for insert mode completion

set smartindent
"set autoindent
set autoread " automatically reload files changed outside Vim
set autowrite " automatically write files when doing things like :make

set cmdheight=2
set laststatus=2
set showcmd
set showfulltag
set shortmess+=ts

if has("gui_running")
    set guioptions-=T
    set lines=60
    set columns=130
endif

set autowriteall
set wildchar=<Tab> wildmenu wildmode=longest:full

set sessionoptions=buffers,curdir,folds,globals,localoptions,options,resize,tabpages,winpos,winsize

let mapleader = ","
set cursorline

" bufmru
"===========================================================================================================
let g:bufmru_switchkey = "<Space>" 

" Configure a nice status line (based on jamessan's?)
"===========================================================================================================
set statusline=
set statusline+=%3.3n\                       " buffer number
set statusline+=%f\                          " file name
set statusline+=%h%1*%m%r%w%0*               " flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}, " filetype
set statusline+=%{strlen(&fenc)?&fenc:&enc}%{&bomb?'/bom':''}, " encoding
set statusline+=%{&fileformat}]              " file format
set statusline+=%{exists('loaded_scmbag')?SCMbag_Info():''} " show vcs status
set statusline+=%=                           " right align
set statusline+=0x%-8B\                      " current char
set statusline+=%-14.(%l,%c%V%)\ %<%P        " offset

" Nicer looking tabs and whitespace
"===========================================================================================================
set encoding=utf-8
if (&termencoding == "utf-8") || has("gui_running")
	if v:version >= 700
		 set listchars=tab:»·,trail:·,extends:…,eol:¶
	else
		 set listchars=tab:»·,trail:·,extends:…,eol:¶
	endif
endif

" Color Scheme 
"===========================================================================================================
" IMPORTANT: Uncomment one of the following lines to force
" using 256 colors (or 88 colors) if your terminal supports it,
" but does not automatically use 256 colors by default.
set t_Co=256
"set t_Co=88
if (&t_Co == 256 || &t_Co == 88) && !has('gui_running') " &&
  "\ filereadable(expand("/usr/share/vim/plugin/guicolorscheme.vim"))
  " Use the guicolorscheme plugin to makes 256-color or 88-color
  " terminal use GUI colors rather than cterm colors.
  "runtime! plugin/guicolorscheme.vim
  "GuiColorScheme wombat
  colorscheme wombat256
else
  " For 8-color 16-color terminals or for gvim, just use the
  " regular :colorscheme command.
  colorscheme colorful
endif

" Key mappings
"===========================================================================================================
imap jj <Esc>
nnoremap <silent> <C-l> :nohl<CR>
nnoremap <silent> <F6> :mak<CR>
nnoremap <silent> <C-F6> :mak clean<CR>
nnoremap <silent> <F2> :NERDTreeToggle<CR>
nnoremap <silent> <F12> :TlistToggle<CR>
set wildcharm=<C-Z>
nnoremap <F10> :b <C-Z>
nnoremap <F11> :e 
nnoremap <C-w>d :Bclose<CR>
"nnoremap <Leader>, :LustyJuggler<CR>

map Y y$
" from spiiph and jamessan on #vim:
nnoremap <expr> gf empty(taglist(expand('<cfile>'))) ? "gf" : ":ta <C-r><C-f><CR>"

" setup git commits to use the git syntax highlighting
"===========================================================================================================
autocmd BufNewFile,BufRead COMMIT_EDITMSG set filetype=gitcommit

" OmniCppComplete
"===========================================================================================================
" -- required --
set nocp " non vi compatible mode
filetype plugin on " enable plugins

" -- configs --
let OmniCpp_MayCompleteDot = 1 " autocomplete with .
let OmniCpp_MayCompleteArrow = 1 " autocomplete with ->
let OmniCpp_MayCompleteScope = 1 " autocomplete with ::
let OmniCpp_SelectFirstItem = 2 " select first item 
let OmniCpp_NamespaceSearch = 2 " search namespaces in this and included files
let OmniCpp_ShowPrototypeInAbbr = 0 " show function prototype (i.e. parameters) in popup window
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 0 
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]

"-- automatically open and close the popup menu / preview window --
"au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menu,preview,longest,menuone

" -- ctags --
" map <ctrl>+F12 to generate ctags for current folder:
map <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR><CR>
" add current directory's generated tags file to available tags
set tags+=./tags
set tags+=/home/catalin/.vim/tags/cpp
set tags+=/home/catalin/.vim/tags/gl

set tagstack
"noremap <C-d> :pop<CR> -- use C-t
"inoremap <C-d> <Esc>:pop<CR> -- use C-t
noremap <C-e> :tag<CR>
inoremap <C-e> <Esc>:tag<CR>
noremap <C-m> <C-]>
inoremap <C-m> <C-]>
noremap <C-b> <Esc>:tag 

" hitting enter with completion open selects the completion and closes preview
inoremap <silent> <expr> <CR> pumvisible() ? "\<C-Y>\<C-O>\<C-W>z" : "\<CR>"
inoremap <silent> <expr> <S-Tab> pumvisible() ? "\<C-P>" : "\<S-Tab>"

" TagList customizations
"===========================================================================================================
let Tlist_Use_Right_Window=1
let Tlist_Enable_Fold_Column=0
let Tlist_Show_One_File=1
let Tlist_Compact_Format=1
let Tlist_Process_File_Always=1
let Tlist_Exit_OnlyWindow=1
let Tlist_GainFocus_On_ToggleOpen=1
set updatetime=1000
au BufEnter __Tag_List__ :setlocal statusline=Tag\ List 

" yankring
"===========================================================================================================
" let yankring_history_file='.yankring'

" vcscommand customization
"===========================================================================================================
let VCSCommandEnableBufferSetup=1

" NERD plugin config
"===========================================================================================================
let NERDTreeShowBookmarks=1
let NERDChristmasTree=1
let NERDShutUp=1 " no more 'unknown filetype' warnings!
let NERDTreeChDirMode=2
autocmd BufEnter * lcd %:p:h

" FuzzyFinder
"===========================================================================================================
nnoremap <silent> <Leader>, :FuzzyFinderBuffer<CR>
nnoremap <silent> <Leader>ff :FuzzyFinderFile<CR>
nnoremap <silent> <Leader>fd :FuzzyFinderDir<CR>
nnoremap <silent> <Leader>fj :FuzzyFinderTextMate<CR>
nnoremap <silent> <Leader>ft :FuzzyFinderTag!<CR>
nnoremap <silent> <Leader>fk :FuzzyFinderMruCmd<CR>
nnoremap <silent> <Leader>fm :FuzzyFinderMruFile<CR>

" window/buffer navigation 
"===========================================================================================================
nnoremap <silent> <C-h> :bp<CR>
nnoremap <silent> <C-j> <C-W>h<CR>
nnoremap <silent> <C-k> <C-W>l<CR>
nnoremap <silent> <C-l> :bn<CR>

"map <silent> j :bp<CR>
"map <silent> <A-j> :bp<CR>
"map <silent> k :bn<CR>
"map <silent> <A-k> :bn<CR>

" session manager
"===========================================================================================================
nnoremap <silent><C-F3> :SessionSave<CR> 
nnoremap <silent><C-F4> :SessionList<CR> 
nnoremap <silent><C-F5> :SessionOpenLast<CR>

" eclim settings
"===========================================================================================================
" Eclim settings
" ,i imports whatever is needed for current line
 nnoremap <silent> <LocalLeader>i :JavaImport<cr>
" ,d opens javadoc for statement in browser
 nnoremap <silent> <LocalLeader>d :JavaDocSearch -x declarations<cr>
" ,<enter> searches context for statement
nnoremap <silent> <LocalLeader><cr> :JavaSearchContext<cr>
" ,jv validates current java file
nnoremap <silent> <LocalLeader>jv :Validate<cr>
" ,jc shows corrections for the current line of java
nnoremap <silent> <LocalLeader>jc :JavaCorrect<cr>
" 'open' on OSX will open the url in the default browser without issue
let g:EclimBrowser='open'

" Supertab settings
"===========================================================================================================
" supertab + eclim == java win
let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabDefaultCompletionTypeDiscovery = [
\ "&completefunc:<c-x><c-u>",
\ "&omnifunc:<c-x><c-o>",
\ ]
let g:SuperTabLongestHighlight = 1


" My functions
"===========================================================================================================

" transpose
"============================================================
function! s:transpose()
    let maxcol = 0
    let lines = getline(1, line('$'))

    for line in lines
        let len = len(line)
        if len > maxcol 
            let maxcol = len
        endif
    endfor

    let newlines = []
    for col in range(0, maxcol - 1)
        let newline = ''
        for line in lines
            let line_with_extra_spaces = printf('%-'.maxcol.'s', line)
            let newline .= line_with_extra_spaces[col]
        endfor
        call add(newlines, newline)
    endfor

    1,$"_d
    call setline(1, newlines)
endfunction

command! TransposeBuffer call s:transpose()

function! Rotate()
python <<EOF
import vim, itertools
max_len = max((len(n) for n in vim.current.buffer))

vim.current.buffer[:] = [
    ''.join(n) for n in itertools.izip(
        *( n + ' ' * (max_len - len(n))
           for n in vim.current.buffer))]
EOF
endfunction

command! Rotate call Rotate()


