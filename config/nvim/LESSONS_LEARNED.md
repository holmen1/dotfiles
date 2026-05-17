# Lessons Learned

Documenting key findings and fixes during Neovim configuration maintenance.

## Treesitter & Neovim 0.12 Compatibility (April 2026)

**Issue:** Terraform preview in Telescope (`leader+sf`) triggered error:
```
attempt to call method 'range' (a nil value)
```
Only occurred when navigating to README.md files.

**Root Cause:** nvim-treesitter on the deprecated `master` branch uses an old API incompatible with Neovim 0.12's updated treesitter runtime.

**Resolution:**
1. Update plugin spec to use `main` branch: `branch = 'main'` in init.lua
2. Reinstall tree-sitter CLI: `cargo install tree-sitter-cli`
3. Reinstall parsers: `:TSUninstall all` → `:TSInstall c lua haskell markdown`
4. Optional: Constrain `ensure_installed` list to prevent bloat from `auto_install`

**Key Takeaway:** Always verify plugin branches match target editor version, especially during major version upgrades.

---

## Leader Key Timing Issues (April 2026)

**Issue:** `<leader>sf` (telescope find files) only worked when typed very quickly, otherwise did nothing.

**Root Cause:** `timeoutlen` was set to 300ms, which was too fast for comfortable human typing of multi-key sequences.

**Resolution:** Increase `timeoutlen` to 700ms in options.lua:
```lua
vim.opt.timeoutlen = 700
```

**Key Takeaway:** When leader key sequences feel "laggy" or require fast typing, increase `timeoutlen` in options.lua. No need for which-key just to manage timing—Neovim's built-in timeout works fine.

---

## Plugin Manager Choice: lazy.nvim vs vim.pack (April 2026)

**Issue:** Experimented with Neovim 0.12's native `vim.pack` system but reverted to lazy.nvim.

**Root Cause:** vim.pack had UX issues (poor update/list commands) and complexity (manual lazy loading, platform-specific build hooks).

**Resolution:** Stick with lazy.nvim for its battle-tested event system, dependency resolution, and ergonomic `:Lazy` commands.

**Key Takeaways:**
- Use `vim.fn.isdirectory()` for directory checks (not deprecated `vim.loop.fs_stat()`)
- Be explicit with lazy loading: `lazy = false` for essential plugins, `event = 'VeryLazy'` for deferrable ones
- Avoid global `defaults = { lazy = true }`—it breaks plugins needing immediate loading

---

## [Add future lessons here]
