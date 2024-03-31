source /usr/share/cachyos-fish-config/cachyos-config.fish

# dotfiles
alias dots="git --git-dir={$HOME}/.dotfiles --work-tree={$HOME}"

# fuzzy finder
fzf --fish | source

# zoxide
zoxide init fish | source
alias cd="z"
