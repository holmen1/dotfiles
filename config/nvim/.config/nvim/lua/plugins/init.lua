-- Plugin specifications for lazy.nvim.

return {
  { 'folke/tokyonight.nvim', priority = 1000, lazy = false, config = function() require('plugins.theme') end }, -- load immediately so colors are ready before UI draws
  { 'nvim-treesitter/nvim-treesitter', branch = 'main', lazy = false, build = ':TSUpdate', config = function() require('plugins.treesitter') end }, -- core editing feature, so load up front
  { 'nvim-lua/plenary.nvim', lazy = true }, -- shared helper library, only load when another plugin needs it
  { 'nvim-telescope/telescope.nvim', event = 'VeryLazy', config = function() require('plugins.telescope') end, dependencies = { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' } },
  { 'nvim-tree/nvim-web-devicons', lazy = true }, -- icons are optional and only needed when a UI component asks for them
  { 'j-hui/fidget.nvim', lazy = false, config = function() require('plugins.fidget') end }, -- notifications and progress indicators for async tasks
  { 'neovim/nvim-lspconfig', lazy = false, config = function() require('plugins.lspconfig') end }, -- LSP wiring should be ready as soon as editing starts
  { 'echasnovski/mini.nvim', lazy = false, config = function() require('plugins.mini') end }, -- statusline/tabline/textobjects are part of the base editing experience
  { 'sitiom/nvim-numbertoggle' }, -- simple plugin: just install it, no setup file needed
  { 'lewis6991/gitsigns.nvim', event = 'VeryLazy', config = function() require('plugins.gitsigns') end },
}

-- vim: ts=2 sts=2 sw=2 et
