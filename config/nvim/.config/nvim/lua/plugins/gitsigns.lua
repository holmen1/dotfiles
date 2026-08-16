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
    local function map(mode, keys, func, desc)
      vim.keymap.set(mode, keys, func, { buffer = bufnr, desc = desc })
    end

    map('n', ']c', function()
      if vim.wo.diff then
        vim.cmd.normal({ ']c', bang = true })
      else
        gitsigns.nav_hunk('next')
      end
    end, 'Next hunk')

    map('n', '[c', function()
      if vim.wo.diff then
        vim.cmd.normal({ '[c', bang = true })
      else
        gitsigns.nav_hunk('prev')
      end
    end, 'Previous hunk')

    map('n', '<leader>gd', gitsigns.preview_hunk_inline, '[G]it [d]iff inline')
    map('n', '<leader>gr', gitsigns.reset_hunk, '[G]it [r]eset hunk')
    map('v', '<leader>gr', function()
      gitsigns.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') })
    end, '[G]it [r]eset hunk')
    map('n', '<leader>gR', gitsigns.reset_buffer, '[G]it [R]eset buffer')
    map('n', '<leader>ga', gitsigns.stage_hunk, '[G]it st[a]ge hunk')
    map('v', '<leader>ga', function()
      gitsigns.stage_hunk({ vim.fn.line('.'), vim.fn.line('v') })
    end, '[G]it st[a]ge hunk')

    local hl = vim.api.nvim_set_hl
    hl(0, 'GitSignsAdd', { fg = '#808000', bg = 'NONE', bold = true })
    hl(0, 'GitSignsChange', { fg = '#FFBF00', bg = 'NONE', bold = true })
    hl(0, 'GitSignsDelete', { fg = '#F44336', bg = 'NONE', bold = true })
  end,
})

-- vim: ts=2 sts=2 sw=2 et
