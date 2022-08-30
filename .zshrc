# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/alexander/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Run commands
alias neofetch="echo;echo;neofetch"
alias nv="nvim"
alias la="exa -la"
alias src="source ${HOME}/.bashrc"

alias cbuild="cargo build"
alias cdoc="cargo doc"
alias crun="cargo run"
alias ctest="cargo test"

alias ~="cd ${HOME}"
alias ..="cd .."

# git
alias dots="git --git-dir=${HOME}/.dotfiles --work-tree=${HOME}"
alias gst="git status"
alias ga="git add"
alias gc="git commit -m "
alias gp="git push"

# vars
export PATH=${PATH}:${HOME}/.local/bin
export EDITOR=/usr/bin/nvim
export VIMRC=${HOME}/.config/nvim/init.vim
export BROWSER=/usr/bin/brave-bin

PROMPT="┌─ %F{cyan}%B%n%b%f ── %F{cyan}%B%D{%I:%M %p}%b%f ── %F{cyan}%B%-1~%b%f ──"$'\n'"└─ %F{red}%B%(!.#.$)%b%f "

#PS1="┌─ ${BLUE}\u${NC} ── ${BLUE}\@${NC} ── ${BLUE}\W${NC} ──\n└─ ${RED}\$${NC} "
#PS2="╶─ ${RED}\$${NC} "
