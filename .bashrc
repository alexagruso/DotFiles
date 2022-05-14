# if shell is non-interactive then exit
if [[ $- != *i* ]] ; then
	return
fi

## run commands

# aliases
alias dots="git --git-dir=${HOME}/dotfiles --work-tree=${HOME}"
alias neofetch="echo;echo;neofetch"
alias nv="nvim"
alias la="exa -la"

alias ~="cd ${HOME}"
alias ..="cd .."

# path
export PATH=${PATH}:${HOME}/.local/bin # local bin

# prompt
RED="\e[1;91m"
BLUE="\e[1;36m"
GREEN="\e[1;32m"
YELLOW="\e[1;33m"
NC="\e[0m"

PS1="┌─ ${BLUE}\u${NC} ── ${BLUE}\@${NC} ── ${BLUE}\W${NC} ──\n└─ ${RED}\$${NC} "
PS2="╶─ ${RED}\$${NC} "
