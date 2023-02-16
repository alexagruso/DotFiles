# exit if shell is non-interactive
if [[ $- != *i* ]] ; then
	return
fi

# general
alias ls="exa"
alias la="exa -la"
alias nv="nvim"
alias pg="ping gnu.org -c 3"
alias ~="cd ~"
alias ..="cd .."

# git
alias g="git"
alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gp="git push"
alias gl="git pull"
alias gr="git restore"
alias grm="git rm"

# dotfiles
alias dots="git --git-dir=${HOME}/.dotfiles --work-tree=${HOME}"

# npm
alias npm="pnpm"
alias npi="pnpm install"
alias npd="pnpm install -D"
alias npr="pnpm run"

# path
export PATH=${PATH}:~/.local/bin/

# prompt colors
RED="\e[1;91m"
BLUE="\e[1;36m"
GREEN="\e[1;32m"
YELLOW="\e[1;33m"
NC="\e[0m"

# ps prompt
PS1="┌─ ${BLUE}\u${NC} ── ${BLUE}\@${NC} ── ${BLUE}\W${NC} ──\n└─ ${RED}\$${NC} "
PS2="╶─ ${RED}\$${NC} "
