require("nvim-tree").setup {
  sort_by = "case_sensitive",
  view = {
    adaptive_size = true,
  },
  filters = {
    dotfiles = true,
  }
}

vim.keymap.set("n", "<C-t>", ":NvimTreeToggle<CR>", { silent = true })
