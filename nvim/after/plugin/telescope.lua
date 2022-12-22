require('telescope').setup {
  pickers = {
    find_files = {
      theme = "dropdown"
    },
    live_grep = {
      theme = "dropdown"
    }
  }
}

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
