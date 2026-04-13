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

## [Add future lessons here]
