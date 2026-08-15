-- Core keymaps.

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set({"n", "t"}, "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" }) -- TODO Enable jump up when in terminal mode
vim.keymap.set("n", "<C-A-j>", ":resize +2<CR>", { desc = "Resize split down" })
vim.keymap.set("n", "<C-A-k>", ":resize -2<CR>", { desc = "Resize split up" })

-- vim: ts=2 sts=2 sw=2 et
