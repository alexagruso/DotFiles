call plug#begin()
    Plug 'itchyny/lightline.vim'
    Plug 'itchyny/vim-gitbranch'
    Plug 'szw/vim-maximizer'
    Plug 'kassio/neoterm'
    Plug 'neovim/nvim-lspconfig'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'nvim-treesitter/nvim-treesitter', {'do': 'TSUpdate'}
    Plug 'folke/tokyonight.nvim'
    Plug 'jiangmiao/auto-pairs'
    Plug 'junegunn/fzf', {'do': { -> fzf#install() }}
    Plug 'junegunn/fzf.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'Yggdroot/indentLine'
call plug#end()

" editor
set number
set relativenumber

set tabstop=4
set shiftwidth=4
set expandtab

set splitright
set splitbelow

set ignorecase
set smartcase
set incsearch

set completeopt=menuone,preview
set nobackup
set nowritebackup
set updatetime=750

let g:mapleader = "\<space>"

" buffers
nnoremap <silent><c-h> :prev<cr>
nnoremap <silent><c-l> :next<cr>
 
" vim-maximizer
nnoremap <silent><leader>w :MaximizerToggle<cr>
vnoremap <silent><leader>w :MaximizerToggle<cr>gv

" neoterm
let g:neoterm_default_mod = 'vertical'
let g:neoterm_size = 60
let g:neoterm_autoinsert = 1
nnoremap <silent><leader>q  :Ttoggle<cr>
tnoremap <silent><leader>q  <c-\><c-n>:Ttoggle<cr>
tnoremap <silent><esc> <c-\><c-n>

" auto-pairs
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<c-b>'

" fzf
nnoremap <silent><leader><space> :GFiles<cr>
if has('nvim')
    au! TermOpen * tnoremap <buffer> <esc> <c-\><c-n>
    au! FileType fzf tunmap <buffer> <esc>
endif

" fugitive
nnoremap <silent><leader>gs :G<cr>

" colorscheme
set termguicolors
let g:tokyonight_style = "night"
colorscheme tokyonight

" lightline
set noshowmode
let g:lightline = {
            \ 'colorscheme': 'wombat',
            \ 'active': {
                \   'left': [ ['mode', 'paste'],
                \             ['gitbranch', 'readonly', 'filename', 'modified'] ]
                \ },
                \ 'component_function': {
                    \   'gitbranch': 'gitbranch#name'
                    \ },
                    \ }

" enable treesitter
lua << EOF
require('nvim-treesitter.configs').setup {
    ensure_installed = {"c", "cpp", "haskell", "rust", "vim"},
    sync_install = false,

    highlight = {
        enable = true,
        },
    }
EOF

" coc
nnoremap <silent> gd <Plug>(coc-definition)
nnoremap <silent> gy <Plug>(coc-type-definition)
nnoremap <silent> gi <Plug>(coc-implementation)
nnoremap <silent> gr <Plug>(coc-references)

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
nnoremap <leader>rn <Plug>(coc-rename)
