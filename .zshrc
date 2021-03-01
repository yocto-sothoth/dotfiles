autoload -Uz promptinit
promptinit
prompt adam1 green green green

setopt extended_history
setopt hist_ignore_alldups
setopt no_beep
setopt share_history

HISTSIZE=10000
SAVEHIST=10000

alias act='ruby ~/fragments/util/act.rb'
alias be='bundle exec'
alias cp='cp -i'
alias e='emacsclient -nw -a ""'
alias h='history -i 1'
alias ke='emacsclient -e "(kill-emacs)"'
alias mv='mv -i'
alias rt='ruby ~/fragments/util/rt.rb'
alias t='tmux'

eval "$(crenv init -)"
eval "$(pyenv init -)"
eval "$(rbenv init -)"

case $OSTYPE in
    darwin*)
        FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

        alias ll='ls -alFG'
        alias ls='ls -FG'

        [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"
        ;;
    linux*)
        FPATH="$(ghq list -p zsh-completions)"/src:$FPATH

        alias ll='ls -alF --color'
        alias ls='ls -F --color'

        [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
        ;;
esac

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select

typeset -U PATH
