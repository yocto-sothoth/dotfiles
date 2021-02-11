# dotfiles

``` shell
$ cd ~
$ git clone https://github.com/yocto-sothoth/dotfiles
```

## Brewfile

``` shell
$ cd ~
$ brew bundle -v --file dotfiles/Brewfile
```

## Symlinks

``` shell
$ cd ~
$ ln -s ~/dotfiles/.pryrc
$ ln -s ~/dotfiles/.tmux.conf
$ ln -s ~/dotfiles/.zshenv
$ ln -s ~/dotfiles/.zshrc

$ cd ~/.emacs.d
$ ln -s ~/dotfiles/init.el
$ ln -s ~/dotfiles/snippets
```

## gpg

``` shell
$ gpg --import xxx
$ gpg --edit-key xxx trust quit
```

## git

``` shell
$ git config --global user.name xxx
$ git config --global user.email xxx
$ git config --global user.signingkey xxx
$ git config --global gpg.program gpg
$ git config --global commit.gpgsign true
$ git config --global init.defaultBranch main
$ git config --global pull.rebase false
$ git config --global core.pager 'less -F'
```

## lesskey

``` shell
$ cd ~
$ lesskey -o .lesskey dotfiles/lesskey
```

## terminfo

``` shell
$ cd ~
$ tic -x -o .terminfo dotfiles/terminfo-custom.src
```

## misc settings

- ssh
- clamav
- rbenv, crenv, pyenv, nvm, etc
