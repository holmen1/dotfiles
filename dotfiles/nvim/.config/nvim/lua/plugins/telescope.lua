-- ~/.config/nvim/lua/plugins/telescope.lua
-- Native configuration for Telescope

local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
  vim.notify("telescope not found! Ensure `vim.pack.add` worked.", vim.log.levels.WARN)
  return
end

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
telescope.setup {
  defaults = {
    mappings = {
      i = {
        ["<C-x>"] = require('telescope.actions').delete_buffer,
      },
      n = {
        ["<C-x>"] = require('telescope.actions').delete_buffer,
      }
    }
  },
}

-- Enable Telescope extensions if they are installed
pcall(telescope.load_extension, 'fzf')

-- See `:help telescope.builtin`
local builtin = require 'telescope.builtin'
vim.keymap.set('n', '<leader>sk', builtin.keymaps, { desc = '[S]earch [K]eymaps' })
vim.keymap.set('n', '<leader>sf', function()
  builtin.find_files {
    hidden = true,
    file_ignore_patterns = { '.git/' }
  } 
end, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', builtin.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sr', builtin.resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader><leader>', builtin.buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>sc', builtin.git_bcommits, { desc = '[S]earch [C]ommit history for current buffer' })

-- vim: ts=2 sts=2 sw=2 et
