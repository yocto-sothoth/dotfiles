export EDITOR='emacsclient -nw -a ""'
export NVM_DIR=$HOME/.nvm
export PATH=$HOME/.crenv/bin:$PATH
export RUBY_THREAD_VM_STACK_SIZE=$((1024**3))
export TERM=xterm-emacs

case $OSTYPE in
    darwin*)
        export GPG_TTY=$(tty)
        export LESSKEY=$HOME/.lesskey
        export PATH=/usr/local/sbin:$PATH
        ;;
    linux*)
        export PYENV_ROOT=$HOME/.pyenv
        export PATH=$PYENV_ROOT/bin:$PATH
        export PATH=$HOME/.rbenv/bin:$PATH
        ;;
esac
