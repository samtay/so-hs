# so [![Build Status](https://travis-ci.org/samtay/so.svg?branch=master)](https://travis-ci.org/samtay/so)

**Note: this is under active development.** The prompt interface is fully
functional, but the more tui-esque sophisticated interface is not finished. The
google parser is out-of-date, so all searches currently use the StackOverflow
API which.. leaves much to be desired.

A terminal interface for Stack Overflow

![terminal-gif](./doc/example.gif)

## install

#### Arch Linux
However you typically install from the
[AUR](https://aur.archlinux.org/packages/so-git/), e.g.
```bash
yay -S so-git # or yaourt -S so-git
```

#### source
Others can install from source. First [get
stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/so.git
cd so
stack install so
```
