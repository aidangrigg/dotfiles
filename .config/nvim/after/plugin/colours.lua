
 function Color(color)

  vim.g.linefly_options = {
    separator_symbol = "⎪",
    progress_symbol = "↓",
    active_tab_symbol = "▪",
    git_branch_symbol = "",
    error_symbol = "E",
    warning_symbol = "W",
    information_symbol = "I",
    tabline = false,
    winbar = false,
    with_file_icon = true,
    with_git_branch = true,
    with_git_status = true,
    with_diagnostic_status = true,
    with_session_status = true,
    with_indent_status = false,
  }

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

Color()
