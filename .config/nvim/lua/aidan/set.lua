vim.opt.guicursor = "i:block"

vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.smarttab = true
vim.opt.autoindent = true
vim.opt.colorcolumn = '80'
vim.opt.signcolumn = 'yes'

vim.opt.wrap = false

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.swapfile = false
vim.opt.backup = false

vim.opt.updatetime = 50
vim.opt.regexpengine = 1

vim.opt.so = 8

vim.opt.mouse = ''
vim.opt.showmode = true

vim.opt.termguicolors = true

vim.opt.foldlevelstart = 99
vim.g.loaded_matchparen = 1

vim.api.nvim_create_autocmd({"TextYankPost"}, {
  callback = function()
    vim.highlight.on_yank{higroup="IncSearch", timeout = 150, on_visual = false}
  end
})
