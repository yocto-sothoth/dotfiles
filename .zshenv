export EDITOR='emacsclient -nw -a ""'
export NVM_DIR="$HOME/.nvm"
export RUBY_THREAD_VM_STACK_SIZE=268435456

case ${OSTYPE} in
    darwin*)
        export GPG_TTY=$(tty)
        export LESSKEY=$HOME/dotfiles/.lesskey
        export PATH=/usr/local/sbin:$PATH
        export TERM=xterm-emacs
        osascript -e 'tell application "System Events" to key code 102'
        ;;
    linux*)
        export PATH=$HOME/.anyenv/bin:$PATH
        export PATH=$HOME/.rbenv/bin:$PATH
        eval "$(anyenv init -)"
        eval "$(rbenv init -)"
        ;;
esac
