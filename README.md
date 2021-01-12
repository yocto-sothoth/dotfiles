# dotfiles

``` shell
$ cd ~
$ git clone https://github.com/yocto-sothoth/dotfiles
```

## Brewfile

``` shell
$ cd ~/dotfiles
$ brew bundle -v
```

## Symlinks

``` shell
$ cd ~
$ ln -s ~/dotfiles/.tmux.conf
$ ln -s ~/dotfiles/.zprofile
$ ln -s ~/dotfiles/.zshenv
$ ln -s ~/dotfiles/.zshrc

$ cd ~/.config/karabiner/assets/complex_modifications
$ ln -s ~/dotfiles/complex_modifications.json

$ cd ~/.emacs.d
$ ln -s ~/dotfiles/init.el
$ ln -s ~/dotfiles/snippets
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
```

## lesskey

``` shell
$ cd ~/dotfiles
$ lesskey -o .lesskey lesskey
```

## terminfo

``` shell
$ cd ~
$ tic -x -o .terminfo dotfiles/terminfo-custom.src
```

## zsh

``` shell
$ echo '/usr/local/bin/zsh' | sudo tee -a /etc/shells > /dev/null
$ chsh -s /usr/local/bin/zsh
```
