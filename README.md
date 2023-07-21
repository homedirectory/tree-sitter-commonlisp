## Tree-sitter for Common Lisp

Currently used by tree-sitter is this grammar: https://github.com/theHamsta/tree-sitter-commonlisp

## Installlation
This is what I have in `after/plugin/treesitter.lua`: 
```lua
local parsers = require("nvim-treesitter.parsers").get_parser_configs()

parsers.commonlisp = {
    install_info = {
        url = "/YOUR_LOCAL_PATH/tree-sitter-commonlisp",
        -- remote should also work
        -- url = "https://github.com/homedirectory/tree-sitter-commonlisp",
        revision = "master",
        files = { "src/parser.c" },
        branch = "master",
    },
}

-- other stuff ...

-- finally, call the main setup function
require "nvim-treesitter.configs".setup {
...
}
```

### Queries
To use queries defined here you need to hack the `nvim-treesitter` plugin.
Replace `~/.local/share/nvim/site/pack/packer/start/nvim-treesitter/queries/commonlisp` by a symlink to the `queries` directory in this repo.

Why? Because `nvim-treesitter` stores queries for all supported languages in the main repo instead of delegating this to the specific parsers used, which would greatly simplify the process of replacing a parser for a given language. 
If you know why `nvim-treesitter` chose to couple the plugin so tightly with specific parsers let me know.
