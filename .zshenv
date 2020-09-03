case ${OSTYPE} in
    linux*)
        export PATH=$HOME/.anyenv/bin:$PATH
        export PATH=$HOME/.rbenv/bin:$PATH
        ;;
    darwin*)
        export PATH=/usr/local/sbin:$PATH
        export PATH=/usr/local/bin:$PATH
        ;;
esac

eval "$(anyenv init -)"
eval "$(rbenv init -)"
