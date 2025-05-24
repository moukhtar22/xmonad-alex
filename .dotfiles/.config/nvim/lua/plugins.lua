require("lazy").setup({

    {
        "folke/tokyonight.nvim",
        lazy = false,
        config = function()
            require('ui.tokyonight')
            vim.cmd([[colorscheme tokyonight]])
        end,
    },

    {
        "nvim-tree/nvim-web-devicons",
        lazy = true,
    },

    {
        "rcarriga/nvim-notify",
    },

    {
        "nvim-lualine/lualine.nvim",
        after = "tokyonight.nvim",
        event = "BufEnter",
        config = function()
            require('ui.lualine')
        end,
    },

    {
        "akinsho/bufferline.nvim",
        version = "*",
        after = "tokyonight.nvim",
        event = "BufEnter",
        config = function ()
            require("ui.bufferline")
        end,
    },

    {
        "L3MON4D3/LuaSnip",
        after = "nvim-cmp",
        config = function ()
            require("functionality.snippets")
        end,
    },

    {
        "Gridex118/neovim-snippets",
        config = function ()
            require("luasnip.loaders.from_vscode").lazy_load()
        end,
    },

    {
        "ggandor/leap.nvim",
        event = "BufEnter",
        config = function ()
            require('functionality.leap')
        end,
    },

    {
        "kylechui/nvim-surround",
        version = '*',
        event = "VeryLazy",
        config = function()
            require('nvim-surround').setup {}
        end,
    },

    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            require('functionality.treesitter')
        end,
    },

    {
        'nvim-treesitter/nvim-treesitter-textobjects',
        after = 'nvim treesitter',
    },

    {
        "nvim-telescope/telescope.nvim",
        config = function ()
            require('ui.telescope')
        end,
        event = "VimEnter",
    },

    {
        "nvim-lua/plenary.nvim",
    },

    {
        "BurntSushi/ripgrep",
    },

    {
        "lukas-reineke/indent-blankline.nvim",
        after = "tokyonight.nvim",
        config = function()
            require('ui.indent_blankline')
        end,
    },

    {
        "stevearc/oil.nvim",
        config = function ()
            require("functionality.oil")
        end,
    },

    {
        "nanotee/zoxide.vim",
    },

    {
        "guns/vim-sexp",
        after = "BufEnter",
    },

    {
        "jiangmiao/auto-pairs",
    },

})
