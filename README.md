# so-hs [![Build Status](https://travis-ci.org/samtay/so-hs.svg?branch=master)](https://travis-ci.org/samtay/so-hs)

**Note:** I've since decided to rewrite this project in Rust
[here](https://github.com/samtay/so). The rewrite has way more features and is
more portable.

![terminal-gif](./doc/example.gif)

This implementation uses a basic prompt interface. The biggest drawback is that
the google parser is out-of-date, so all searches currently use the
StackOverflow API, which leaves much to be desired.

## install
Installation on MacOS and Arch Linux is simple, just follow the instructions
below. Otherwise you can install from
[source](#install-from-source).

#### MacOS
Installation on a Mac is simple with Homebrew:
```bash
brew install samtay/tui/so-hs
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
git clone https://github.com/samtay/so-hs.git
cd so-hs
stack install so
```
