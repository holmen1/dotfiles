require('fidget').setup({
  notification = {
    override_vim_notify = true,
    window = {
      done_ttl = 5,
    },
    configs = {
      default = {
        name = false, -- hide group name
        icon = false, -- hide group icon (the <<)
      }
    }
  },
})

require('telescope').load_extension('fidget')
