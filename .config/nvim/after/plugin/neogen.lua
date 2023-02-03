require('neogen').setup {
  languages = {
    ['svelte'] = require('neogen.configurations.javascript')
  },
}

local opts = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<Leader>dc", ":lua require('neogen').generate({ type = 'class' })<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>df", ":lua require('neogen').generate({ type = 'func' })<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>dt", ":lua require('neogen').generate({ type = 'type' })<CR>", opts)
