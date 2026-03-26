-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Set to true if you have a Nerd Font installed and selected in the terminal
vim.g.have_nerd_font = true

-- [[ Setting options ]]
require 'options'

-- [[ Basic Keymaps ]]
require 'keymaps'

-- [[ Neovim 0.12 Plugin Hooks ]]
-- Handle post-install/update build scripts natively
vim.api.nvim_create_autocmd('PackChanged', {
  callback = function(ev)
    if ev.data.spec.name == 'nvim-treesitter' and (ev.data.kind == 'install' or ev.data.kind == 'update') then
      vim.cmd('TSUpdateSync')
    end
  end
})

-- [[ Neovim 0.12 Plugin Downloads ]]
vim.pack.add({
  'https://github.com/folke/tokyonight.nvim',
  'https://github.com/nvim-treesitter/nvim-treesitter',
})

-- [[ Initialize Plugins ]]

-- Set Colorscheme
vim.cmd('packadd tokyonight.nvim')
require('plugins.theme')

-- Treesitter
vim.cmd('packadd nvim-treesitter')
require('plugins.treesitter')

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
