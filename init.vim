" ==========================================================
" PLUGINS
" ==========================================================
call plug#begin()

" terminal
Plug 'voldikss/vim-floaterm'

" git
Plug 'f-person/git-blame.nvim' " git blame
Plug 'airblade/vim-gitgutter' " git gutter

" syntax highlighting & colorscheme
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} " tree sitter
Plug 'itchyny/lightline.vim' " status bar
Plug 'ryanoasis/vim-devicons' " developer icons
Plug 'rafi/awesome-vim-colorschemes' " colorschemes
Plug 'ntpeters/vim-better-whitespace' " highlighting trailing whitepsace

" lsp
Plug 'neoclide/coc.nvim' , {'do': 'yarn install --frozen-lockfile'} " autocomplete
let g:coc_global_extensions = ['coc-tslint-plugin', 'coc-tsserver', 'coc-css', 'coc-html', 'coc-json', 'coc-prettier', 'coc-svelte']

" code navigation
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim' " telescope
Plug 'preservim/nerdtree' " file tree
Plug 'preservim/tagbar' " tagbar for code navigation

" other useful things
Plug 'JoosepAlviste/nvim-ts-context-commentstring' " commenting for files that have multiple filetypes inside of them (.svelte)
Plug 'jiangmiao/auto-pairs' " Auto Pairing brackets and quotes
Plug 'tpope/vim-commentary' " For Commenting gcc & gc
Plug 'tpope/vim-surround' " Surrounding ysw)
Plug 'ap/vim-css-color' " CSS Color Preview

call plug#end()

" ==========================================================
" SETTINGS
" ==========================================================

" line numbers
:set number
:set relativenumber

" indentation
:set autoindent
:set expandtab
:set tabstop=2
:set shiftwidth=2
:set smarttab
:set autoindent

" line wrapping
:set nowrap

" search settings
:set ignorecase
:set smartcase

" line folding
:set so=7
:set foldmethod=syntax
:set foldlevel=99
:set foldclose=all
:set nofoldenable

" leader remapping
:let mapleader = "\<Space>"

" colourscheme
let g:lightline = {
	\ 'colorscheme': 'gruvbox',
	\ }
set background=dark
set termguicolors
colorscheme gruvbox

" ==========================================================
" PLUGIN CONFIG
" ==========================================================
lua <<EOF
-- treesitter
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  },
  context_commentstring = {
    enable = true
  }
}
-- telescope
require('telescope').setup {
	defaults = {
		vimgrep_arguments = {
			'rg', '--hidden', '--no-heading', '--with-filename', '--line-number', '--column', '--smart-case'
		}
	}
}

EOF

let g:NERDTreeDirArrowExpandable="+"
let g:NERDTreeDirArrowCollapsible="~"

" ==========================================================
" KEYMAPS
" ==========================================================

" \nh no highlights
nnoremap <Leader>nh <cmd>noh<cr>

" nerdtree
nnoremap <C-t> :NERDTreeToggle<CR>

" git blame
nnoremap <Leader>gb <cmd>GitBlameToggle<cr>

" floaterm
nnoremap <Leader>nt <cmd>FloatermNew! --width=0.8 --height=0.8<cr>
nnoremap <Leader>tt <cmd>FloatermToggle --width=0.8 --height=0.8<cr>
nnoremap <Leader>gi <cmd>FloatermNew --width=0.8 --height=0.8 lazygit<cr>

tnoremap <C-h> <cmd>FloatermPrev<cr>
tnoremap <C-l> <cmd>FloatermNext<cr>
tnoremap <C-\> <cmd>FloatermToggle<cr>

" telescope config
nnoremap <Leader>ff <cmd>Telescope find_files<cr>
nnoremap <Leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" tagbar
nnoremap <Leader>tb <cmd>TagbarToggle<CR>

" coc config

" tab autocompletion
inoremap <expr> <Tab> coc#pum#visible() ? coc#_select_confirm() : "<Tab>"
inoremap <silent><expr> <c-space> coc#refresh()

" ctrl-j & k move up and down autocomplete prompts
inoremap <silent><expr> <C-j>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? false :
      \ coc#refresh()
inoremap <expr><C-k> coc#pum#visible() ? coc#pum#prev(1) : false

" goto code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" applying codeaction to the selected region.
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" apply autofix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

