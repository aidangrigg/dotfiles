-- vim-markdown
vim.g.vim_markdown_folding_disabled = 1
vim.g.vim_markdown_conceal = 1

-- Neorg
require('neorg').setup {
  load = {
    ["core.defaults"] = {}, -- Loads default behaviour
    ["core.esupports.metagen"] = {
      config = {
        type = "auto"
      }
    },
    ["core.concealer"] = {  -- Adds pretty icons to your documents
      config = {
        icon_preset = "diamond",
      }
    },
    ["core.dirman"] = { -- Manages Neorg workspaces
      config = {
        workspaces = {
          work = "~/notes/work",
          personal = "~/notes/personal",
        },
        default_workspace = "work"
      },
    },
  },
}
