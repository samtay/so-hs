# so [![Build Status](https://travis-ci.org/samtay/so.svg?branch=master)](https://travis-ci.org/samtay/so)

**Note: this is under active development.** The prompt interface is fully
functional, but the more tui-esque sophisticated interface is not finished. The
google parser is out-of-date, so all searches currently use the StackOverflow
API which.. leaves much to be desired.

A terminal interface for Stack Overflow

![terminal-gif](./doc/example.gif)

## install
Installation on MacOS and Arch Linux is simple, just follow the instructions
below. Other Linux distros can try the [github release
binary](https://github.com/samtay/so/releases/download/0.1.0/so-debian-x86_64),
but no guarantees. Otherwise you can install from
[source](#install-from-source).

#### MacOS
Installation on a Mac is simple with Homebrew:
```bash
brew install samtay/tui/so
```

#### Arch Linux
However you typically install from the
[AUR](https://aur.archlinux.org/packages/so-git/), e.g.
```bash
yay -S so-git # or yaourt -S so-git
```

#### install from source
First [get
stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/so.git
cd so
stack install so
```
