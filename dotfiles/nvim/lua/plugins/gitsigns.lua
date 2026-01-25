-- Minimal gitsigns.nvim config: gutter signs only, no keymaps
return {
  'lewis6991/gitsigns.nvim',
  opts = {
    signs = {
      add          = { text = '+' },
      change       = { text = '~' },
      delete       = { text = '_' },
      topdelete    = { text = '‾' },
      changedelete = { text = '~' },
    },
    on_attach = function(bufnr)
      -- Keymaps
      local gs = package.loaded.gitsigns
      vim.keymap.set('n', '<leader>gd', gs.diffthis, { buffer = bufnr, desc = '[G]it [D]iff this file' })

      -- Custom highlight colors
      local hl = vim.api.nvim_set_hl
      hl(0, 'GitSignsAdd',    { fg = '#808000', bg = 'NONE', bold = true }) -- opulent olive
      hl(0, 'GitSignsChange', { fg = '#FFBF00', bg = 'NONE', bold = true }) -- ambiguous amber
      hl(0, 'GitSignsDelete', { fg = '#7C0A02', bg = 'NONE', bold = true }) -- bold bordeaux
    end,
  },
}
