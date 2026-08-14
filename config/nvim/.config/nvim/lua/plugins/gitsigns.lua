local gitsigns = require('gitsigns')

gitsigns.setup({
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
  on_attach = function(bufnr)
    local function map(keys, func, desc)
      vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
    end

    map(']c', function()
      if vim.wo.diff then
        vim.cmd.normal({ ']c', bang = true })
      else
        gitsigns.nav_hunk('next')
      end
    end, 'Next hunk')

    map('[c', function()
      if vim.wo.diff then
        vim.cmd.normal({ '[c', bang = true })
      else
        gitsigns.nav_hunk('prev')
      end
    end, 'Previous hunk')

    map('<leader>gd', gitsigns.preview_hunk_inline, '[G]it [D]iff inline')
    map('<leader>gr', gitsigns.reset_hunk, '[G]it [r]eset hunk')
    map('<leader>gR', gitsigns.reset_buffer, '[G]it [R]eset buffer')

    local hl = vim.api.nvim_set_hl
    hl(0, 'GitSignsAdd', { fg = '#808000', bg = 'NONE', bold = true })
    hl(0, 'GitSignsChange', { fg = '#FFBF00', bg = 'NONE', bold = true })
    hl(0, 'GitSignsDelete', { fg = '#7C0A02', bg = 'NONE', bold = true })
  end,
})

-- vim: ts=2 sts=2 sw=2 et
