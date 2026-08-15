vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('nvim-lsp-attach', { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc, mode)
      vim.keymap.set(mode or 'n', keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
    end

    local builtin = require('telescope.builtin')
    map('gd', builtin.lsp_definitions, '[G]oto [D]efinition')
    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
    map('grr', builtin.lsp_references, '[G]oto [R]eferences')
    map('gri', builtin.lsp_implementations, '[G]oto [I]mplementation')

    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
      map('<leader>th', function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
      end, '[T]oggle Inlay [H]ints')
    end
  end,
})

vim.diagnostic.config({
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
})

local servers = { 'clangd', 'hls' }
if jit.os ~= 'BSD' then
  table.insert(servers, 'asm_lsp')
  table.insert(servers, 'lua_ls')
  table.insert(servers, 'bashls')
end

vim.lsp.config('clangd', {
  init_options = { fallbackFlags = { '--std=gnu99' } },
})

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

vim.lsp.enable(servers)

-- vim: ts=2 sts=2 sw=2 et
