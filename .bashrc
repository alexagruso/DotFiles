# if shell is non-interactive then exit
if [[ $- != *i* ]] ; then
	return
fi

# run commands

# aliases
alias dots="git --git-dir=${HOME}/dotfiles --work-tree=${HOME}"
alias neofetch="echo;echo;neofetch"
alias nv="nvim"
alias la="exa -la"
alias ~="cd ${HOME}"

# path
export PATH=${PATH}:${HOME}/.local/bin # local bin
