require('mini.ai').setup({ n_lines = 500 })
require('mini.surround').setup()

local statusline = require('mini.statusline')
statusline.setup({ use_icons = vim.g.have_nerd_font })

statusline.section_location = function()
  return '%2l:%-2v'
end

require('mini.tabline').setup()

vim.keymap.set('n', '<C-l>', '<Cmd>bnext<CR>', { noremap = true, silent = true, desc = 'Next buffer' })
vim.keymap.set('n', '<C-h>', '<Cmd>bprevious<CR>', { noremap = true, silent = true, desc = 'Previous buffer' })

-- vim: ts=2 sts=2 sw=2 et
