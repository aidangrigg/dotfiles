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

  -- make the background actually black
  vim.api.nvim_set_hl(0, 'Normal', { bg = "#000000" })

  -- this plugin doesnt set a statusline color so set it manually
  vim.api.nvim_set_hl(0, 'StatusLine', { fg = "#EBEBEB", bg = "#101010" })

  -- background transparency
  vim.api.nvim_set_hl(0, 'Normal', { bg = "none" })
  vim.api.nvim_set_hl(0, 'NormalFloat', { bg = "none" })
end

function Zenbones(color)
  vim.g.zenbones_compact = 1
  color = color or "zenbones"
  vim.opt.background = "dark"
  vim.cmd.colorscheme(color)
end

function VimMonochrome()
  vim.g.monochrome_italic_comments = 1

  vim.cmd.colorscheme("monochrome")

  vim.api.nvim_set_hl(0, 'Normal', { bg = "#111111" })
  vim.api.nvim_set_hl(0, 'NormalFloat', { bg = "#111111" })
  vim.api.nvim_set_hl(0, 'SignColumn', { bg = "none" })
  vim.api.nvim_set_hl(0, 'SignColumn', { bg = "none" })
  vim.api.nvim_set_hl(0, 'PMenu', { bg = "#222222" })
  vim.api.nvim_set_hl(0, 'PMenuSel', { bg = "#56514e", fg = "White" })
end

VimMonochrome()

-- Monochrome()
