require('fidget').setup({
  -- Options related to LSP progress subsystem
  progress = {
    -- Options related to how LSP progress messages are displayed as notifications
    display = {
      done_ttl = 5, -- How long a message should persist after completion
    },
  },
  notification = {
    override_vim_notify = true,
    configs = {
      default = {
        name = false, -- hide group name
        icon = false, -- hide group icon (the <<)
      }
    }
  },
})

require('telescope').load_extension('fidget')
