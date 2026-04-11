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

-- [[ Bootstrap lazy.nvim ]]
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if vim.fn.isdirectory(lazypath) == 0 then
  vim.fn.system({
    'git', 'clone', '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- [[ Plugin Specs ]]
local plugins = {
  { 'folke/tokyonight.nvim', priority = 1000, lazy = false, config = function() require('plugins.theme') end },
  { 'nvim-treesitter/nvim-treesitter', lazy = false, build = ':TSUpdate', config = function() require('plugins.treesitter') end },
  { 'nvim-lua/plenary.nvim', lazy = true },
  { 'nvim-telescope/telescope.nvim', event = 'VeryLazy', config = function() require('plugins.telescope') end, dependencies = { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' } },
  { 'nvim-telescope/telescope-ui-select.nvim', lazy = true },
  { 'nvim-tree/nvim-web-devicons', lazy = true },
  { 'folke/lazydev.nvim', lazy = true },
  { 'neovim/nvim-lspconfig', lazy = false, config = function() require('plugins.lspconfig') end, dependencies = { 'folke/lazydev.nvim', 'j-hui/fidget.nvim' } },
  { 'j-hui/fidget.nvim', lazy = true },
  { 'folke/which-key.nvim', lazy = false, config = function() require('plugins.which-key') end },
  { 'echasnovski/mini.nvim', lazy = false, config = function() require('plugins.mini') end },
  { 'lewis6991/gitsigns.nvim', event = 'VeryLazy', config = function() require('plugins.gitsigns') end },
}

require('lazy').setup(plugins)

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
