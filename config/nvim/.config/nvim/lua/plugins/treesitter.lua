-- ~/.config/nvim/lua/plugins/treesitter.lua
-- Native configuration for nvim-treesitter

local status_ok, configs = pcall(require, "nvim-treesitter")
if not status_ok then
  vim.notify("nvim-treesitter not found! Ensure `vim.pack.add` worked.", vim.log.levels.WARN)
  return
end

-- [[ Configure Treesitter ]] See `:help nvim-treesitter`
configs.setup {
  ensure_installed = { 'c', 'lua', 'haskell', 'markdown' },
  auto_install = true,
  highlight = { enable = true },
  -- Disable treesitter indenting for C because it conflicts with Neovim's robust native cindent
  indent = { enable = true, disable = { 'c', 'cpp' } },
}

-- You can manually run `:TSUpdate` after initial installation
-- vim: ts=2 sts=2 sw=2 et
