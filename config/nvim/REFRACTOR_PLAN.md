# Neovim Refactor Plan

Goal: keep the functionality working, but make it understandable enough that each piece has a clear reason to exist.

## Current shape

- `init.lua` now handles startup orchestration and lazy bootstrap.
- `lua/core/options.lua` holds editor options.
- `lua/core/keymaps.lua` holds global keymaps.
- `lua/core/autocmds.lua` holds autosave and yank highlighting.
- `lua/plugins/init.lua` holds the lazy.nvim plugin list.
- `lua/plugins/*.lua` holds plugin-specific setup.
- `lua/custom/terminal.lua` adds a terminal toggle.
- `lazy-bootstrap.lua` and `lazy-plugins.lua` have been removed.
- `README.md` documents the setup, but some of it may drift from the actual code over time.

## Refactor goals

1. Keep the same useful behavior: LSP, Telescope, Treesitter, Gitsigns, terminal toggle, and core keymaps.
2. Make startup flow obvious.
3. Separate "core Neovim settings" from "plugin behavior".
4. Remove duplicate or stale entry points.
5. Keep plugin config small and readable.
6. Document what each keymap and plugin does in plain language.
7. Prefer incremental changes so the editor keeps working after each step.
8. Keep editor plugins in `lazy.nvim`, but keep runtime tools like LSP servers out of Neovim and installed by the system package manager.

## Proposed target structure

- `init.lua` should only orchestrate startup.
- `lua/core/options.lua` for editor options.
- `lua/core/keymaps.lua` for global keymaps.
- `lua/core/autocmds.lua` for autocommands.
- `lua/plugins/` for one file per plugin or plugin group.
- `lua/custom/` for small local utilities that do not belong to a plugin.
- `README.md` for user-facing usage notes.
- `REFRACTOR_PLAN.md` for the work plan while the overhaul is in progress.

## Phases

### Phase 1: Inventory

- [x] Read every Neovim file and write down what it does.
- [x] Mark each module as core, plugin config, or utility.
- [x] Identify stale files, duplicate setup, and comments that no longer match reality.

### Phase 2: Simplify entry points

- [x] Keep one bootstrap path.
- [x] Decide whether `lazy-bootstrap.lua` and `lazy-plugins.lua` stay, merge, or go.
- [x] Make `init.lua` a short, readable entry point.

### Phase 3: Rebuild by layer

- [x] Move core settings into clearly named modules.
- [x] Keep plugin setup grouped by purpose:
  - appearance
  - navigation
  - LSP
  - git
  - editing helpers
  - terminal/custom tools
- [x] Add short comments only where a choice is non-obvious.
- [x] Prefer the smallest practical toolset; avoid adding plugins when built-in Neovim or an existing dependency already covers the job.
- [x] Do not chase bleeding-edge features if a stable, simpler path exists.
- [x] Do not add Mason or similar tool managers unless there is a concrete maintenance problem that system packages cannot solve.

### Plugin cleanup progress

- [x] Simplified theme, Treesitter, Telescope, LSP, Gitsigns, Mini, and terminal modules.
- [x] Removed the old `pcall(... not found ...)`/`vim.pack.add` style messages from plugin setup.
- [x] Kept the existing feature set intact while reducing setup noise.

### Documentation cleanup progress

- [x] Restore a practical README instead of a minimal placeholder.
- [x] Keep the Appendix as a long-form Vim reference.
- [x] Add newcomer-friendly terminal usage notes.
- [x] Keep the README aligned with the current module layout and keymaps.

### Phase 4: Document while simplifying

- Rewrite README sections to match the real keymaps and plugins.
- Add a short "why this exists" note for anything unusual.
- Keep lessons learned in `LESSONS_LEARNED.md`.

### Phase 5: Verify

- Reopen Neovim and make sure startup still works.
- Check that keymaps still exist.
- Confirm LSP, Telescope, Treesitter, Gitsigns, and the terminal toggle still behave as expected.

## Learning goals

- Understand the startup order.
- Understand which options are global and which are plugin-specific.
- Learn how lazy.nvim loads plugins.
- Learn which keymaps are essential versus convenience bindings.
- Learn which parts of the config are "my preferences" versus "necessary plumbing".

## Done means

- The structure is readable without needing memory of past edits.
- There is one obvious path through the config.
- The README explains the current behavior.
- The config is smaller, clearer, and easier to change safely.
- The useful behavior is preserved even if the underlying architecture changes.
- The setup stays minimal and maintainable instead of accumulating extra tools.

## Next step

- Do a final pass for any remaining stale comments or confusing wording in plugin modules and README.
