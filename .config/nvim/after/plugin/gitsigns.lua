require('gitsigns').setup()

vim.keymap.set("n", '<leader>hn', '<CMD>Gitsigns next_hunk<CR><CMD>Gitsigns preview_hunk_inline<CR>');
vim.keymap.set("n", '<leader>hp', '<CMD>Gitsigns prev_hunk<CR><CMD>Gitsigns preview_hunk_inline<CR>');

