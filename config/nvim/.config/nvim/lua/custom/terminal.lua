local M = {}
local terminal_buf
local terminal_win

function M.toggle()
  local open = terminal_buf
    and vim.api.nvim_buf_is_valid(terminal_buf)
    and terminal_win
    and vim.api.nvim_win_is_valid(terminal_win)

  if open then
    vim.api.nvim_win_close(terminal_win, true)
    terminal_buf = nil
    terminal_win = nil
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
