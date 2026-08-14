# Adding a new plugin

This Neovim config uses `lazy.nvim`, and the plugin list lives in `lua/plugins/init.lua`.

## Usual workflow

1. Open `config/nvim/.config/nvim/lua/plugins/init.lua`.
2. Add a plugin spec to the returned table.
3. Restart Neovim.
4. Run `:Lazy sync` to install it.

## Simple example: `sitiom/nvim-numbertoggle`

`nvim-numbertoggle` is a small plugin that only toggles line numbers, so it does not need extra setup code.

Add this line to the plugin list:

```lua
{ 'sitiom/nvim-numbertoggle' },
```

That is enough for a simple plugin:

- the repository name tells `lazy.nvim` what to install
- no separate config file is needed
- no extra keymaps or options are required

## When to add a separate file

If a plugin needs setup, keep the spec in `lua/plugins/init.lua` and move the configuration into a separate file under `lua/plugins/`.

Example:

```lua
{ 'author/plugin-name', config = function() require('plugins.plugin-name') end },
```

Then put the setup code in `lua/plugins/plugin-name.lua`.

Use a separate file when the plugin:

- has more than a couple of options
- adds keymaps
- needs autocommands or special load logic
- deserves a short explanation

Keep one-line plugins inline.

## Notes

- `lazy.nvim` updates `lazy-lock.json` when you sync or update plugins.
- Commit the lockfile if you want plugin versions pinned.
- If a new plugin does not appear, check `:Lazy` and make sure the spec is in `lua/plugins/init.lua`.

