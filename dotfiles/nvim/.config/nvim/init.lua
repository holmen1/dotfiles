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
    local name = ev.data.spec.name
    local kind = ev.data.kind
    if (kind == 'install' or kind == 'update') then
      -- Treesitter Parser Compilation
      if name == 'nvim-treesitter' then
        vim.cmd('TSUpdateSync')
      end
      -- Telescope FZF Native C-Extension Compilation
      if name == 'telescope-fzf-native.nvim' then
        vim.system({ 'make' }, { cwd = ev.data.path }):wait()
      end
    end
  end
})

-- [[ Neovim 0.12 Plugin Downloads ]]
vim.pack.add({
  'https://github.com/folke/tokyonight.nvim',
  'https://github.com/nvim-treesitter/nvim-treesitter',
  'https://github.com/nvim-lua/plenary.nvim',
  'https://github.com/nvim-telescope/telescope.nvim',
  'https://github.com/nvim-telescope/telescope-fzf-native.nvim',
  'https://github.com/nvim-telescope/telescope-ui-select.nvim',
  'https://github.com/nvim-tree/nvim-web-devicons',
})

-- [[ Initialize Plugins ]]

-- Set Colorscheme
vim.cmd('packadd tokyonight.nvim')
require('plugins.theme')

-- Treesitter
vim.cmd('packadd nvim-treesitter')
require('plugins.treesitter')

-- Telescope
vim.cmd('packadd plenary.nvim')
vim.cmd('packadd telescope.nvim')
vim.cmd('packadd telescope-fzf-native.nvim')
vim.cmd('packadd telescope-ui-select.nvim')
vim.cmd('packadd nvim-web-devicons')
require('plugins.telescope')

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
