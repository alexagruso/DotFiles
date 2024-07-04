# exit if shell is non-interactive
if [[ $- != *i* ]] ; then
	return
fi

# general
alias ls="exa"
alias la="exa -la"
alias nv="nvim"
alias pg="ping gnu.org -c 3"
alias src="source ~/.bashrc"
alias ~="cd ~"
alias ..="cd .."

# system
alias sysup="sudo emerge --update --newuse --deep @world"

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

# misc
alias s="sudo"

# path
export PATH=${PATH}:~/.local/bin/
