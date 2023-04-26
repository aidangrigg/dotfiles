vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.conceallevel = 2

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "$", "g$")
vim.keymap.set("n", "^", "g^")

vim.keymap.set("v", "j", "gj")
vim.keymap.set("v", "k", "gk")
vim.keymap.set("v", "$", "g$")
vim.keymap.set("v", "^", "g^")

vim.keymap.set("n", "<leader>mp", ":MarkdownPreviewToggle<CR>")
