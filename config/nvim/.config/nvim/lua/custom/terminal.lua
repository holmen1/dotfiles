local M = {}
local terminal_buf
local terminal_win

function M.toggle()
  local buf_valid = terminal_buf and vim.api.nvim_buf_is_valid(terminal_buf)
  local win_valid = terminal_win and vim.api.nvim_win_is_valid(terminal_win)

  if win_valid then
    vim.api.nvim_win_close(terminal_win, false)
    terminal_win = nil
    return
  end

  if buf_valid then
    vim.cmd.split()
    vim.cmd('buffer ' .. terminal_buf)
    vim.cmd.resize(15)
    terminal_win = vim.api.nvim_get_current_win()
    vim.cmd.startinsert()
    return
  end

  vim.cmd.split()
  vim.cmd.terminal()
  vim.cmd.resize(15)

  terminal_buf = vim.api.nvim_get_current_buf()
  terminal_win = vim.api.nvim_get_current_win()

  vim.keymap.set('t', '<Esc>', '<C-\\><C-n>', { buffer = terminal_buf, noremap = true })
  vim.cmd.startinsert()
end

vim.keymap.set('n', '<leader>tt', M.toggle, { noremap = true, silent = true, desc = 'Toggle terminal' })

return M

-- vim: ts=2 sts=2 sw=2 et
