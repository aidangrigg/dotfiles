
 function GruvboxLight(color)

  vim.g.gruvbox_material_lightline_disable_bold = 1
  vim.g.gruvbox_material_background = 'medium'
  vim.g.gruvbox_material_foreground = 'material'
  vim.g.gruvbox_material_better_performance = 1

  color = color or "gruvbox-material"
  vim.cmd.colorscheme(color)

  vim.g.gruvbox_material_background = 'medium'
  vim.g.gruvbox_material_foreground = 'material'
  vim.g.gruvbox_material_better_performance = 1
  vim.g.gruvbox_material_ui_contrast = 'high'

  color = color or "gruvbox-material"
  vim.cmd.colorscheme(color)
end

function Monochrome(color)
  color = color or "monochrome"
  vim.opt.background = "dark"
  vim.cmd.colorscheme(color)
end

function Zenbones(color)
  vim.g.zenbones_compact = 1
  color = color or "zenbones"
  vim.opt.background = "dark"
  vim.cmd.colorscheme(color)
end

function Color(color)
  Zenbones()
end

Color()
