
function color(color)
  vim.g.gruvbox_material_background = 'hard'
  vim.g.gruvbox_material_foreground = 'material'
  vim.g.gruvbox_material_better_performance = 1

  color = color or "gruvbox-material"
  vim.cmd.colorscheme(color)
end

color()
