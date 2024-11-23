return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'cljoly/bepo.nvim'
    use 'neovim/nvim-lspconfig'
    use {
	    'VonHeikemen/lsp-zero.nvim' ,
	    branch = 'v1.x',
	    requires = {
		    {'neovim/nvim-lspconfig'},
		    {'hrsh7th/nvim-cmp'},
		    {'hrsh7th/cmp-nvim-lsp'},
		    {'L3MON4D3/LuaSnip'},
	    }
    }
end)
