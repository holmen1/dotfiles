-- ~/.config/nvim/lua/plugins/lspconfig.lua
-- Native configuration for LSP, Lazydev and Fidget

-- Setup lazydev for Lua completion and neovim APIs
local has_lazydev, lazydev = pcall(require, "lazydev")
if has_lazydev then
  lazydev.setup({})
end

-- Setup fidget for LSP status updates
local has_fidget, fidget = pcall(require, "fidget")
if has_fidget then
  fidget.setup({})
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

    -- Find references for the word under your cursor.
    map('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')

    -- Jump to the implementation of the word under your cursor.
    map('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')

    -- Jump to the type of the word under your cursor.
    map('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')

    -- WARN: This is not Goto Definition, this is Goto Declaration.
    --  For example, in C this would take you to the header.
    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

    -- Rename the variable under your cursor.
    --  Most Language Servers support renaming across files, etc.
    map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

    -- Execute a code action, usually your cursor needs to be on top of an error
    -- or a suggestion from your LSP for this to activate.
    map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction', { 'n', 'x' })

    -- Fuzzy find all the symbols in your current document.
    map('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')

    -- Fuzzy find all the symbols in your current workspace.
    map('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
    
    -- Format current buffer or visual selection
    map('<leader>f', function()
      vim.lsp.buf.format { async = true }
    end, 'Format code', { 'n', 'v' })

    -- The following two autocommands are used to highlight references of the
    -- word under your cursor when your cursor rests there for a little while.
    -- When you move your cursor, the highlights will be cleared.
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) then
      local highlight_augroup = vim.api.nvim_create_augroup('kickstart-lsp-highlight', { clear = false })
      vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        buffer = event.buf,
        group = highlight_augroup,
        callback = vim.lsp.buf.document_highlight,
      })

      vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
        buffer = event.buf,
        group = highlight_augroup,
        callback = vim.lsp.buf.clear_references,
      })

      vim.api.nvim_create_autocmd('LspDetach', {
        group = vim.api.nvim_create_augroup('kickstart-lsp-detach', { clear = true }),
        callback = function(event2)
          vim.lsp.buf.clear_references()
          vim.api.nvim_clear_autocmds { group = 'kickstart-lsp-highlight', buffer = event2.buf }
        end,
      })
    end

    -- Toggle inlay hints keymap
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
  virtual_text = {
    source = 'if_many',
    spacing = 2,
    format = function(diagnostic)
      local diagnostic_message = {
        [vim.diagnostic.severity.ERROR] = diagnostic.message,
        [vim.diagnostic.severity.WARN] = diagnostic.message,
        [vim.diagnostic.severity.INFO] = diagnostic.message,
        [vim.diagnostic.severity.HINT] = diagnostic.message,
      }
      return diagnostic_message[diagnostic.severity]
    end,
  },
}

-- LSP servers - install via system package manager:
local servers = {
  asm_lsp = 'asm-lsp',
  clangd = 'clangd',
  hls = 'haskell-language-server-wrapper',
  lua_ls = 'lua-language-server',
}

-- Only enable servers that are installed on the OS
local available = {}
local missing = {}
for server, cmd in pairs(servers) do
  if vim.fn.executable(cmd) == 1 then
    table.insert(available, server)
  else
    table.insert(missing, cmd .. ' (' .. server .. ')')
  end
end

if #missing > 0 then
  vim.defer_fn(function()
    vim.notify('LSP: not found: ' .. table.concat(missing, ', '), vim.log.levels.WARN)
  end, 500)
end

-- Configure servers using Neovim 0.12+ native API!
-- (nvim-lspconfig simply provides the default backend data for these)
vim.lsp.config('clangd', {
  init_options = {
    fallbackFlags = { '--std=c99' },
  },
})

vim.lsp.config('lua_ls', {
  settings = {
    Lua = {
      completion = {
        callSnippet = 'Replace',
      },
      diagnostics = {
        globals = { 'vim' },
      },
      workspace = {
        checkThirdParty = false,
      },
    },
  },
})

-- Enable only available servers natively
vim.lsp.enable(available)

-- vim: ts=2 sts=2 sw=2 et
