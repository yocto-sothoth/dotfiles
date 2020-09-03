autoload -Uz promptinit
promptinit
prompt adam1 black

setopt histignorealldups sharehistory
bindkey -e

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history

autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

export EDITOR=emacs
export RUBY_THREAD_VM_STACK_SIZE=268435456

alias e='emacs -nw'
alias t='tmux'
alias h='history 1'
alias mv='mv -i'
alias cp='cp -i'
alias be='bundle exec'
alias cop='rubocop --require rubocop-performance'
alias eixt='exit'

case ${OSTYPE} in
    linux*)
        eval "$(dircolors -b)"
        alias ls='ls -F --color'
        alias ll='ls -alF --color'
        ;;
    darwin*)
        export GPG_TTY=$(tty)
        alias ls='ls -FG'
        alias ll='ls -alFG'
        ;;
esac
