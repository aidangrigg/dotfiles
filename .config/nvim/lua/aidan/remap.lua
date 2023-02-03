vim.g.mapleader = " "

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "<leader>nh", ":noh<CR>", { silent = true })

vim.keymap.set("v", ">", ">gv");
vim.keymap.set("v", "<", "<gv");

vim.keymap.set("n", "<leader>lg", ":tabnew<CR>:term lazygit<CR>a", { silent = true })
