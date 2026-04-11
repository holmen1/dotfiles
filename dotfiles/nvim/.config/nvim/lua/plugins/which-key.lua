-- ~/.config/nvim/lua/plugins/which-key.lua
-- Native configuration for Which-Key

local status_ok, wk = pcall(require, "which-key")
if not status_ok then
  vim.notify("which-key not found! Ensure `vim.pack.add` worked.", vim.log.levels.WARN)
  return
end

wk.setup {
  -- delay between pressing a key and opening which-key (milliseconds)
  -- this setting is independent of vim.opt.timeoutlen
  delay = 0,
  icons = {
    -- set icon mappings to true if you have a Nerd Font
    mappings = vim.g.have_nerd_font,
    -- Use default Nerd Font icons (empty table) since we always have a Nerd Font
    keys = {},
  },

  -- Document existing key chains
  spec = {
    { '<leader>s', group = '[S]earch' },
    { '<leader>t', group = '[T]oggle' },
    { '<leader>h', group = 'Git [H]unk', mode = { 'n', 'v' } },
  },
}

-- vim: ts=2 sts=2 sw=2 et