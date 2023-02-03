-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use 'alexghergh/nvim-tmux-navigation'

  use 'tpope/vim-fugitive'

  use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
  use 'nvim-tree/nvim-web-devicons'
  use 'sainnhe/gruvbox-material'
  use 'ntpeters/vim-better-whitespace'

  use 'nvim-lua/plenary.nvim'
  use 'nvim-tree/nvim-tree.lua'

  use 'JoosepAlviste/nvim-ts-context-commentstring'
  use 'jiangmiao/auto-pairs'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'ap/vim-css-color'

  use {
	  'nvim-telescope/telescope.nvim', tag = '0.1.0',
	  -- or                            , branch = '0.1.x',
	  requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- LSP
  use {
	  'VonHeikemen/lsp-zero.nvim',
	  requires = {
		  -- LSP Support
		  {'neovim/nvim-lspconfig'},
		  {'williamboman/mason.nvim'},
		  {'williamboman/mason-lspconfig.nvim'},

		  -- Autocompletion
		  {'hrsh7th/nvim-cmp'},
		  {'hrsh7th/cmp-buffer'},
		  {'hrsh7th/cmp-path'},
		  {'saadparwaiz1/cmp_luasnip'},
		  {'hrsh7th/cmp-nvim-lsp'},
		  {'hrsh7th/cmp-nvim-lua'},

		  -- Snippets
		  {'L3MON4D3/LuaSnip'},
		  {'rafamadriz/friendly-snippets'},
	  }
  }

  use 'tamago324/nlsp-settings.nvim'
  use('jose-elias-alvarez/null-ls.nvim')
  use('MunifTanjim/prettier.nvim')

  use {
    "danymat/neogen",
    requires = "nvim-treesitter/nvim-treesitter",
    -- Uncomment next line if you want to follow only stable versions
    tag = "*"
  }

  use {
    'lewis6991/gitsigns.nvim',
    -- tag = 'release' -- To use the latest release (do not use this if you run Neovim nightly or dev builds!)
  }

  use {"shortcuts/no-neck-pain.nvim", tag = "*" }

	use 'bluz71/nvim-linefly'

end)
