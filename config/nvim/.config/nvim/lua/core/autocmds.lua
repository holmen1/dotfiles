-- Core autocommands.

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("nvim-highlight-yank", { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})

vim.api.nvim_create_autocmd({ "InsertLeave", "TextChanged" }, {
  pattern = "*",
  callback = function()
    if vim.bo.modifiable and vim.bo.modified and vim.bo.buftype == "" then
      vim.cmd("silent! write")
    end
  end,
})

-- vim: ts=2 sts=2 sw=2 et
