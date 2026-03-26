-- ~/.config/nvim/lua/plugins/theme.lua
-- Native configuration for TokyoNight colorscheme

-- Ensure the plugin is available before attempting to configure it
local status_ok, tokyonight = pcall(require, "tokyonight")
if not status_ok then
  vim.notify("Tokyonight colorscheme not found! Ensure `vim.pack.add` worked.", vim.log.levels.WARN)
  return
end

-- Configure the colorscheme
tokyonight.setup({
  transparent = true,
  styles = {
    comments = { italic = false }, -- Disable italics in comments
  },
})

-- Load the colorscheme
vim.cmd.colorscheme("tokyonight")
