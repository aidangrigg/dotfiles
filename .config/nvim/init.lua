local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "

require("lazy").setup({
  {
    'alexghergh/nvim-tmux-navigation',
    config = function()
      require('nvim-tmux-navigation').setup {
        disable_when_zoomed = false,
        keybindings = {
          left = "<C-h>",
          down = "<C-j>",
          up = "<C-k>",
          right = "<C-l>",
          last_active = "<C-\\>",
          next = "<C-Space>",
        }
      }
    end
  },
  {
    'stevearc/oil.nvim',
    config = function()
      require('oil').setup {
        columns = {
          "permissions",
          "size"
        },
        view_options = {
          show_hidden = true,
        }
      }
      vim.keymap.set("n", "<leader>ft", ":Oil<CR>")
    end,
    keys = "<leader>ft"
  },
  {
    'tpope/vim-fugitive',
    config = function()
      vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
    end
  },
  -- colourscheme
  -- 'kdheepak/monochrome.nvim',
  'ellisonleao/gruvbox.nvim',
  'robertmeta/nofrils',
  'fxn/vim-monochrome',
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        auto_install = true,
        highlight = { enable = true, additional_vim_regex_highighting = false },
        indent = { enable = true },
      })
    end
  },
  {
    "windwp/nvim-autopairs",
    config = function() require("nvim-autopairs").setup {} end
  },
  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  },
  {
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup {}
    end
  },

  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' }
  },
  -- LSP
  {
    'VonHeikemen/lsp-zero.nvim',
    dependencies = {
      -- LSP Support
      'neovim/nvim-lspconfig',
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Autocompletion
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-path',
      'saadparwaiz1/cmp_luasnip',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',

      -- Snippets
      'L3MON4D3/LuaSnip',
      'rafamadriz/friendly-snippets',
    },
  },
  {
    "danymat/neogen",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
  },
  {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup()
    end
  },
  'renerocksai/telekasten.nvim',

  {
    "iamcco/markdown-preview.nvim",
    build = "cd app && npm install",
    init = function() vim.g.mkdp_filetypes = { "markdown" } end,
  },

  {
    'preservim/vim-markdown',
    dependencies = { "godlygeek/tabular" }
  },
  'junegunn/limelight.vim',
  'folke/zen-mode.nvim',

  {
    'codethread/qmk.nvim',
    config = function()
      ---@type qmk.UserConfig
      local conf = {
        name = 'LAYOUT_split_3x6_3', -- identify your layout name
        layout = {
          'x x x x x x _ x x x x x x',
          'x x x x x x _ x x x x x x',
          'x x x x x x _ x x x x x x',
          '_ _ _ x x x _ x x x _ _ _',
        },
      }
      require('qmk').setup(conf)
    end
  },
})

require("aidan")
