# if shell is non-interactive then exit
if [[ $- != *i* ]] ; then
	return
fi

## run commands

# aliases
alias neofetch="echo;echo;neofetch"
alias nv="nvim"
alias la="exa -la"
alias src="source ${HOME}/.bashrc"

alias cb="cargo build"
alias cr="cargo run"

alias ~="cd ${HOME}"
alias ..="cd .."

# git
alias dots="git --git-dir=${HOME}/dotfiles --work-tree=${HOME}"
alias gst="git status"
alias ga="git add"
alias gc="git commit -m "
alias gp="git push"

# set
export PATH=${PATH}:${HOME}/.local/bin # local bin
export EDITOR=/usr/bin/nvim
export VIMRC=${HOME}/.config/nvim/init.vim

# prompt
RED="\e[1;91m"
BLUE="\e[1;36m"
GREEN="\e[1;32m"
YELLOW="\e[1;33m"
NC="\e[0m"

PS1="┌─ ${BLUE}\u${NC} ── ${BLUE}\@${NC} ── ${BLUE}\W${NC} ──\n└─ ${RED}\$${NC} "
PS2="╶─ ${RED}\$${NC} "
. "$HOME/.cargo/env"
