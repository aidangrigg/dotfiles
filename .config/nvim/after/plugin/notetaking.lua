-- vim-markdown
vim.g.vim_markdown_folding_disabled = 1
vim.g.vim_markdown_conceal = 1
vim.g.vim_markdown_toc_autofit = 1
vim.g.vim_markdown_new_list_item_indent = 0

vim.keymap.set("n", "<leader>zm", ":ZenMode<CR>", { silent = true })
vim.keymap.set("n", "<leader>ni", ":Neorg index<CR>", { silent = true })

-- Telekasten
require('telekasten').setup{
  home = vim.fn.expand("~/notes/"),
  template_new_note = vim.fn.expand("~/notes/templates/base.md")
}

-- Launch panel if nothing is typed after <leader>z
vim.keymap.set("n", "<leader>zz", "<cmd>Telekasten panel<CR>")

-- Most used functions
vim.keymap.set("n", "<leader>zf", "<cmd>Telekasten find_notes<CR>")
vim.keymap.set("n", "<leader>zg", "<cmd>Telekasten search_notes<CR>")
vim.keymap.set("n", "<leader>zd", "<cmd>Telekasten goto_today<CR>")
vim.keymap.set("n", "<leader>zn", "<cmd>Telekasten new_note<CR>")
vim.keymap.set("n", "<leader>zb", "<cmd>Telekasten show_backlinks<CR>")
vim.keymap.set("n", "<leader>zI", "<cmd>Telekasten insert_img_link<CR>")
vim.keymap.set("n", "<leader>zl", "<cmd>Telekasten insert_link<CR>")
vim.keymap.set("n", "gl", "<cmd>Telekasten follow_link<CR>")

-- this isnt working for some reason
vim.api.nvim_set_hl(0, 'tkLink', { bold = true })
vim.api.nvim_set_hl(0, 'tkTag', { bold = true })

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
