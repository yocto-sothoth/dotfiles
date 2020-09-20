export EDITOR=emacs
export RUBY_THREAD_VM_STACK_SIZE=268435456

case ${OSTYPE} in
    linux*)
        export PATH=$HOME/.anyenv/bin:$PATH
        export PATH=$HOME/.rbenv/bin:$PATH
        eval "$(anyenv init -)"
        eval "$(rbenv init -)"
        ;;
    darwin*)
        export GPG_TTY=$(tty)
        export PATH=/usr/local/sbin:$PATH
        ;;
esac
