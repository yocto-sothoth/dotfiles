autoload -Uz promptinit
promptinit
prompt adam1 green green green

setopt hist_ignore_alldups
setopt no_beep
setopt share_history

HISTFILE=~/dotfiles/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

alias be='bundle exec'
alias cp='cp -i'
alias h='history 1'
alias mv='mv -i'
alias t='tmux'

case ${OSTYPE} in
    linux*)
        alias ll='ls -alF --color'
        alias ls='ls -F --color'
        FPATH="$(ghq list -p zsh-completions)"/src:$FPATH
        ;;
    darwin*)
        alias ll='ls -alFG'
        alias ls='ls -FG'
        FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
        ;;
esac

autoload -Uz compinit
compinit -d ~/dotfiles/.zcompdump

zstyle ':completion:*' menu select
