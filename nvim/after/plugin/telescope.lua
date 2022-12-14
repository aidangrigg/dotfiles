-- CONFIGURATION

local telescope = require("telescope")
local telescopeConfig = require("telescope.config")

-- Clone the default Telescope configuration
local vimgrep_arguments = { unpack(telescopeConfig.values.vimgrep_arguments) }

-- I want to search in hidden/dot files.
table.insert(vimgrep_arguments, "--hidden")
-- I don't want to search in the `.git` directory.
table.insert(vimgrep_arguments, "--glob")
table.insert(vimgrep_arguments, "!**/.git/*")

telescope.setup({
	defaults = {
		-- `hidden = true` is not supported in text grep commands.
		vimgrep_arguments = vimgrep_arguments,
	},
	pickers = {
		find_files = {
			-- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
			find_command = { "rg", "--files", "--hidden", "--glob", "!**/.git/*" },
		},
	},
})

-- THEMING

local file_theme = require('telescope.themes').get_dropdown({
  borderchars = {
    { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
    prompt = {"─", "│", " ", "│", '┌', '┐', "│", "│"},
    results = {"─", "│", "─", "│", "├", "┤", "┘", "└"},
    preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
  },
  width = 0.8,
  previewer = false,
  prompt_title = false
})

local full_theme = {
  width = 0.8;
  show_line = false;
  prompt_title = false;
  borderchars = {
    { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
    prompt = {"─", "│", "─", "│", '┌', '┐', "┘", "└"},
    results = {"─", "│", "─", "│", '┌', '┐', "┘", "└"},
    preview = {"─", "│", "─", "│", '┌', '┐', "┘", "└"},
  },
}

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', function() builtin.find_files(file_theme) end)
vim.keymap.set('n', '<leader>fg', function() builtin.live_grep(full_theme) end)
