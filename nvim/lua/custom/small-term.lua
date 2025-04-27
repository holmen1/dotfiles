vim.api.nvim_create_autocmd('TermOpen', {
    group = vim.api.nvim_create_augroup('custom-term-open', {clear = true}),
    callback = function()
        vim.opt.number = false
        vim.opt.relativenumber = false
    end,
})

local job_id = 0

-- Function to open a terminal and store the job ID
local function open_terminal(cmd)
    vim.cmd.new()
    vim.cmd.terminal(cmd)
    vim.cmd.wincmd('J')
    vim.api.nvim_win_set_height(0, 12)
    job_id = vim.bo.channel
end

-- Function to send commands to the terminal
local function send_to_terminal(command)
    if job_id ~= 0 and vim.fn.jobwait({job_id}, 0)[1] == -1 then
        vim.fn.chansend(job_id, { command .. "\r\n" })
    else
        print("No terminal session active")
    end
end

-- Function to check if the current file has a .hs extension
local function is_haskell_file(filename)
    return filename:match('%.hs$') ~= nil
end

-- Function to get the current filename
local function get_current_filename()
    return vim.fn.expand('%')
end

-- Keybinding to open a terminal
vim.keymap.set('n', '<leader>st', function()
    open_terminal()
end, { noremap = true, silent = true, desc = "Open vertical split terminal" })

-- Keybinding to open ghci in a terminal
vim.keymap.set('n', '<leader>ghci', function()
    local filename = get_current_filename()
    if not is_haskell_file(filename) then
        print("Warning: Current file is not a Haskell file (.hs)")
        return
    end
    open_terminal("ghci " .. filename)
end, { noremap = true, silent = true, desc = "Open vertical split terminal with ghci" })

-- Keybinding to load the current Haskell file in ghci
vim.keymap.set('n', '<leader>load', function()
    local filename = get_current_filename()
    if not is_haskell_file(filename) then
        print("Warning: Current file is not a Haskell file (.hs)")
        return
    end
    send_to_terminal(":l " .. filename)
end, { noremap = true, silent = true, desc = "Load current file in ghci" })
