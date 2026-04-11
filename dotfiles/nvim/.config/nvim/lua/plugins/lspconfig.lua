-- ~/.config/nvim/lua/plugins/lspconfig.lua
-- Native configuration for LSP, Lazydev and Fidget

-- Setup fidget for LSP status updates
local has_fidget, fidget = pcall(require, "fidget")
if has_fidget then
  fidget.setup({
    notification = {
      override_vim_notify = true,
    },
  })
end

-- LSP attach autocommand to bind keymaps and highlights when a server attaches
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
  callback = function(event)
    -- We create a function that lets us more easily define mappings specific
    -- for LSP related items. It sets the mode, buffer and description for us each time.
    local map = function(keys, func, desc, mode)
      mode = mode or 'n'
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
    end

    -- Jump to the definition of the word under your cursor.
    --  This is the most common way to navigate code.
    map('gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')

    -- WARN: This is not Goto Definition, this is Goto Declaration.
    --  For example, in C this would take you to the header.
    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

    -- Find references for the word under your cursor.
    map('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')

    -- Jump to the implementation of the word under your cursor.
    -- In Haskell, finds all instances of a type class.
    -- In interface-based languages (Go, C#), finds all implementations of an interface.
    map('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')

    -- Rename the variable under your cursor.
    --  Most Language Servers support renaming across files, etc.
    map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

    -- Execute a code action, usually your cursor needs to be on top of an error
    -- or a suggestion from your LSP for this to activate.
    map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction', { 'n', 'x' })
    
    -- Format current buffer or visual selection
    map('<leader>f', function()
      vim.lsp.buf.format { async = true }
    end, 'Format code', { 'n', 'v' })

    -- Toggle inlay hints keymap
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
      map('<leader>th', function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
      end, '[T]oggle Inlay [H]ints')
    end
  end,
})

-- Diagnostic Config
vim.diagnostic.config {
  severity_sort = true,
  float = { border = 'rounded', source = 'if_many' },
  underline = { severity = vim.diagnostic.severity.ERROR },
  signs = vim.g.have_nerd_font and {
    text = {
      [vim.diagnostic.severity.ERROR] = '󰅚 ',
      [vim.diagnostic.severity.WARN] = '󰀪 ',
      [vim.diagnostic.severity.INFO] = '󰋽 ',
      [vim.diagnostic.severity.HINT] = '󰌶 ',
    },
  } or {},
  virtual_text = { source = 'if_many', spacing = 2 },
}

-- LSP servers - define all, Neovim will silently skip unavailable ones
local servers = { 'clangd', 'hls' }

-- Add servers only available on non-BSD systems
if jit.os ~= 'BSD' then
  table.insert(servers, 'asm_lsp')
  table.insert(servers, 'lua_ls')
  table.insert(servers, 'bashls')
end

-- Configure servers using Neovim 0.12+ native API
vim.lsp.config('clangd', {
  init_options = { fallbackFlags = { '--std=c99' } },
})

-- Configure servers only available on non-BSD systems
if jit.os ~= 'BSD' then
  vim.lsp.config('lua_ls', {
    settings = {
      Lua = {
        completion = { callSnippet = 'Replace' },
        diagnostics = { globals = { 'vim' } },
        workspace = { checkThirdParty = false },
      },
    },
  })
end

-- Enable all configured servers
vim.lsp.enable(servers)

-- vim: ts=2 sts=2 sw=2 et
