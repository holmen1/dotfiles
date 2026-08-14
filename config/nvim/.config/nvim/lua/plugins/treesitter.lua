local configs = require('nvim-treesitter')

configs.setup({
  ensure_installed = { 'c', 'lua', 'haskell', 'markdown' },
  auto_install = true,
  highlight = { enable = true },
  indent = {
    enable = true,
    disable = { 'c', 'cpp' },
  },
})

-- vim: ts=2 sts=2 sw=2 et
