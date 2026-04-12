-- ~/.config/nvim/lua/custom/terminal.lua
-- Minimal custom terminal toggle for bottom split

local M = {}
local terminal_buf = nil
local terminal_win = nil

function M.toggle()
  if terminal_buf and vim.api.nvim_buf_is_valid(terminal_buf) and terminal_win and vim.api.nvim_win_is_valid(terminal_win) then
    -- Terminal exists and is valid, close it
    vim.api.nvim_win_close(terminal_win, true)
    terminal_win = nil
    terminal_buf = nil
  else
    -- Open new terminal at bottom
    vim.cmd('split')
    vim.cmd('terminal')
    vim.cmd('resize 15')
    terminal_buf = vim.api.nvim_get_current_buf()
    terminal_win = vim.api.nvim_get_current_win()

    -- Exit terminal mode with Esc or <C-\><C-n>
    vim.keymap.set('t', '<Esc>', '<C-\\><C-n>', { buffer = terminal_buf, noremap = true })


    vim.cmd('startinsert')
  end
end

-- Keymap to toggle terminal
vim.keymap.set('n', '<leader>tt', M.toggle, { noremap = true, silent = true, desc = 'Toggle terminal' })

return M

-- vim: ts=2 sts=2 sw=2 et
