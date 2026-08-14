-- Core keymaps.

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })
vim.keymap.set("n", "<A-h>", ":vertical resize -2<CR>", { desc = "Resize split left" })
vim.keymap.set("n", "<A-l>", ":vertical resize +2<CR>", { desc = "Resize split right" })
vim.keymap.set("n", "<A-j>", ":resize +2<CR>", { desc = "Resize split down" })
vim.keymap.set("n", "<A-k>", ":resize -2<CR>", { desc = "Resize split up" })

-- vim: ts=2 sts=2 sw=2 et
