local tokyonight = require('tokyonight')

tokyonight.setup({
  transparent = true,
  styles = {
    comments = { italic = false },
  },
})

vim.cmd.colorscheme("tokyonight")

-- vim: ts=2 sts=2 sw=2 et
