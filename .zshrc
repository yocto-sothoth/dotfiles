autoload -Uz promptinit
promptinit
prompt adam1 green

setopt correct
setopt hist_ignore_alldups
setopt no_beep
setopt share_history

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

alias be='bundle exec'
alias cp='cp -i'
alias h='history 1'
alias mv='mv -i'
alias t='tmux'

case ${OSTYPE} in
    linux*)
        alias ll='ls -alF --color'
        alias ls='ls -F --color'
        FPATH=~/.zsh-completions/src:$FPATH
        ;;
    darwin*)
        alias ll='ls -alFG'
        alias ls='ls -FG'
        FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
        ;;
esac

autoload -Uz compinit
compinit

zstyle ':completion:*' menu select
