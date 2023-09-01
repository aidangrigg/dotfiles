-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use 'alexghergh/nvim-tmux-navigation'
  use 'stevearc/oil.nvim'
  use 'tpope/vim-fugitive'

  -- colorschemes
  use 'sainnhe/gruvbox-material'
  use 'kdheepak/monochrome.nvim'
  use {
    "mcchrish/zenbones.nvim",
    requires = "rktjmp/lush.nvim"
  }

  use 'nvim-treesitter/nvim-treesitter'

  use 'ntpeters/vim-better-whitespace'


  use {
    'nvim-telescope/telescope.nvim',
    tag = '*',
    requires = { { 'nvim-lua/plenary.nvim' } }
  }

  -- LSP
  use {
    'VonHeikemen/lsp-zero.nvim',
    requires = {
      -- LSP Support
      { 'neovim/nvim-lspconfig' },
      { 'williamboman/mason.nvim' },
      { 'williamboman/mason-lspconfig.nvim' },

      -- Autocompletion
      { 'hrsh7th/nvim-cmp' },
      { 'hrsh7th/cmp-path' },
      { 'saadparwaiz1/cmp_luasnip' },
      { 'hrsh7th/cmp-nvim-lsp' },
      { 'hrsh7th/cmp-nvim-lua' },

      -- Snippets
      { 'L3MON4D3/LuaSnip' },
      { 'rafamadriz/friendly-snippets' },
    }
  }

  use('jose-elias-alvarez/null-ls.nvim')
  use('MunifTanjim/prettier.nvim')

  use {
    "danymat/neogen",
    requires = "nvim-treesitter/nvim-treesitter",
    tag = "*"
  }


  -- Notetaking
 use {
    "nvim-neorg/neorg",
    run = ":Neorg sync-parsers",
    requires = "nvim-lua/plenary.nvim",
  }

  use 'renerocksai/telekasten.nvim'

  use({
    "iamcco/markdown-preview.nvim",
    run = "cd app && npm install",
    setup = function() vim.g.mkdp_filetypes = { "markdown" } end,
  })

  use 'preservim/vim-markdown'
  use 'junegunn/limelight.vim'
  use 'folke/zen-mode.nvim'
  use 'mzlogin/vim-markdown-toc'

  use {
    'codethread/qmk.nvim',
    config = function()
      ---@type qmk.UserConfig
      local conf = {
        name = 'LAYOUT_split_3x6_3',       -- identify your layout name
        layout = {
          'x x x x x x _ x x x x x x',
          'x x x x x x _ x x x x x x',
          'x x x x x x _ x x x x x x',
          '_ _ _ x x x _ x x x _ _ _',
        },
      }
      require('qmk').setup(conf)
    end
  }

  use {
  "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  }
  use 'miversen33/netman.nvim'
end)
