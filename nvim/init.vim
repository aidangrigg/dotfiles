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
Plug 'nvim-tree/nvim-web-devicons' " icons
Plug 'sainnhe/gruvbox-material' " colorscheme
Plug 'ntpeters/vim-better-whitespace' " highlighting trailing whitepsace
Plug 'lukas-reineke/indent-blankline.nvim'

" lsp
Plug 'neoclide/coc.nvim' , {'do': 'yarn install --frozen-lockfile'} " autocomplete

" code navigation
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim' " telescope
Plug 'nvim-tree/nvim-tree.lua' " file explorer
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
:set colorcolumn=80

" line wrapping
:set nowrap

" search settings
:set ignorecase
:set smartcase

" line folding
:set so=7
:set foldmethod=manual
:set foldlevel=99
:set foldclose=all
:set nofoldenable

" leader remapping
:let mapleader = "\<Space>"


" colourscheme
set background=dark
set termguicolors

let g:lightline = {
  \ 'colorscheme': 'gruvbox_material',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'absolutepath', 'modified' ] ],
  \ }
  \ }

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_better_performance = 1
let g:gruvbox_material_enable_bold = 1
let g:gruvbox_material_dim_inactive_windows = 1

colorscheme gruvbox-material

set mouse=
set noshowmode
set updatetime=100

" ==========================================================
" PLUGIN CONFIG
" ==========================================================
lua <<EOF
-- treesitter
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false
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

require("indent_blankline").setup {
}

require("nvim-web-devicons").setup {
  -- globally enable different highlight colors per icon (default to true)
  -- if set to false all icons will have the default icon's color
  color_icons = true;
  -- globally enable default icons (default to false)
  -- will get overriden by `get_icons` option
  default = true;
}

require("nvim-tree").setup({
  sort_by = "case_sensitive",
  view = {
    adaptive_size = true,
    mappings = {
      list = {
        { key = "9", action = "tabnew" },
      },
    },
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})

EOF

let g:tagbar_ctags_bin="/usr/bin/ctags"

let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0
" ==========================================================
" KEYMAPS
" ==========================================================

nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" \nh no highlights
nnoremap <Leader>nh <cmd>noh<cr>

" nerdtree
nnoremap <C-t> :NvimTreeToggle<CR>

" git blame
nnoremap <Leader>gb <cmd>GitBlameToggle<cr>

" floaterm
nnoremap <Leader>nt <cmd>FloatermNew! --width=0.8 --height=0.8<cr>
nnoremap <Leader>tt <cmd>FloatermToggle<cr>
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
inoremap <expr><silent><Tab> coc#pum#visible() ? coc#_select_confirm() : "<Tab>"
inoremap <expr><silent><c-space> coc#refresh()

" ctrl-j & k move up and down autocomplete prompts
inoremap <expr><silent><C-j> coc#pum#visible() ? coc#pum#next(1) : false
inoremap <expr><silent><C-k> coc#pum#visible() ? coc#pum#prev(1) : false

" goto code navigation.
nmap <silent> gd :call CocAction('jumpDefinition')<CR>
nmap <silent> gvd :call CocAction('jumpDefinition', 'vsplit')<CR>
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

