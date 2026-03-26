-- ~/.config/nvim/lua/plugins/gitsigns.lua
-- Native configuration for gitsigns.nvim

local status_ok, gitsigns = pcall(require, "gitsigns")
if not status_ok then
  vim.notify("gitsigns not found! Ensure `vim.pack.add` worked.", vim.log.levels.WARN)
  return
end

gitsigns.setup({
  signs = {
    add          = { text = '+' },
    change       = { text = '~' },
    delete       = { text = '_' },
    topdelete    = { text = '‾' },
    changedelete = { text = '~' },
  },
  on_attach = function(bufnr)
    -- Navigation
    vim.keymap.set('n', ']c', function()
      if vim.wo.diff then
        vim.cmd.normal({ ']c', bang = true })
      else
        gitsigns.nav_hunk('next')
      end
    end, { buffer = bufnr, desc = 'Next [c]hange (git hunk)' })

    vim.keymap.set('n', '[c', function()
      if vim.wo.diff then
        vim.cmd.normal({ '[c', bang = true })
      else
        gitsigns.nav_hunk('prev')
      end
    end, { buffer = bufnr, desc = 'Previous [c]hange (git hunk)' })

    -- Keymaps
    vim.keymap.set('n', '<leader>gd', gitsigns.preview_hunk_inline, { buffer = bufnr, desc = '[G]it [D]iff inline' })
    vim.keymap.set('n', '<leader>gr', gitsigns.reset_hunk, { buffer = bufnr, desc = '[G]it [r]eset hunk' })
    vim.keymap.set('n', '<leader>gR', gitsigns.reset_buffer, { buffer = bufnr, desc = '[G]it [R]eset buffer' })

    -- Custom highlight colors
    local hl = vim.api.nvim_set_hl
    hl(0, 'GitSignsAdd',    { fg = '#808000', bg = 'NONE', bold = true }) -- opulent olive
    hl(0, 'GitSignsChange', { fg = '#FFBF00', bg = 'NONE', bold = true }) -- ambiguous amber
    hl(0, 'GitSignsDelete', { fg = '#7C0A02', bg = 'NONE', bold = true }) -- bold bordeaux
  end,
})
