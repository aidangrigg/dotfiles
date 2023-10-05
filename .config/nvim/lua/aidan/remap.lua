local opts = { silent = true }
vim.g.mapleader = " "

vim.keymap.set("n", "<C-d>", "<C-d>zz", opts)
vim.keymap.set("n", "<C-u>", "<C-u>zz", opts)

vim.keymap.set("n", "<leader>nh", ":noh<CR>", opts)

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", opts)
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", opts)

vim.keymap.set("v", ">", ">gv", opts)
vim.keymap.set("v", "<", "<gv", opts)

-- greatest remap ever
vim.keymap.set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

vim.keymap.set({"n", "v"}, "<leader>d", [["_d]])

-- buffer things
-- <leader>bk opens telescope buffer list (defined in ../../after/plugin/telescope.lua)
vim.keymap.set("n", "<leader>bl", ":bn<CR>", opts) -- next buffer
vim.keymap.set("n", "<leader>bh", ":bp<CR>", opts) -- prev buffer
vim.keymap.set("n", "<leader>bn", ":enew<CR>", opts) -- new buffer
vim.keymap.set("n", "<leader>bd", ":bd<CR>", opts) -- buffer close

