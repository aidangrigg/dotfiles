-- vim-markdown
vim.g.vim_markdown_folding_disabled = 1
vim.g.vim_markdown_conceal = 1

vim.keymap.set("n", "<leader>zm", ":ZenMode<CR>", { silent = true })
vim.keymap.set("n", "<leader>ni", ":Neorg index<CR>", { silent = true })

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

vim.api.nvim_create_autocmd({"BufEnter", "BufWinEnter"}, {
  pattern = {"*.norg"},
  command = "set conceallevel=3"
})

vim.api.nvim_create_autocmd({"BufEnter", "BufWinEnter"}, {
  pattern = {"*.norg"},
  command = "set concealcursor=n"
})

-- -- Telescope workspace picker for Neorg
--
-- local pickers = require("telescope.pickers")
-- local finders = require("telescope.finders")
-- local conf = require("telescope.config").value
--
-- print(conf)
--
-- local find_neorg_workspace = function (opts)
--   opts = opts or {}
--   pickers.new(opts, {
--     prompt_title = "colors",
--     finder = finders.new_table {
--       results = { "red", "green","blue" }
--     },
--   }):find()
-- end


