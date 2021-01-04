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
alias e='emacsclient -nw -a ""'
alias h='history 1'
alias kille='emacsclient -e "(kill-emacs)"'
alias mv='mv -i'
alias t='tmux'

case $OSTYPE in
    darwin*)
        alias ll='ls -alFG'
        alias ls='ls -FG'
        FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
        ;;
    linux*)
        alias ll='ls -alF --color'
        alias ls='ls -F --color'
        FPATH="$(ghq list -p zsh-completions)"/src:$FPATH
        ;;
esac

autoload -Uz compinit
compinit -d ~/dotfiles/.zcompdump

zstyle ':completion:*' menu select

[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
