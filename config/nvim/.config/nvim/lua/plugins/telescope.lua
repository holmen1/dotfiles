local telescope = require('telescope')
local actions = require('telescope.actions')
local builtin = require('telescope.builtin')

telescope.setup({
  defaults = {
    mappings = {
      i = {
        ["<C-x>"] = actions.delete_buffer,
      },
      n = {
        ["<C-x>"] = actions.delete_buffer,
      },
    },
  },
})

pcall(telescope.load_extension, 'fzf')

vim.keymap.set('n', '<leader>sk', builtin.keymaps, { desc = '[S]earch [K]eymaps' })
vim.keymap.set('n', '<leader>sf', function()
  builtin.find_files({
    hidden = true,
    file_ignore_patterns = { '.git/' },
  })
end, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', builtin.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sr', builtin.resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader><leader>', builtin.buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>sc', builtin.git_bcommits, { desc = '[S]earch [C]ommit history for current buffer' })

-- vim: ts=2 sts=2 sw=2 et
