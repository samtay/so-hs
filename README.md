# so-hs [![Build Status](https://travis-ci.org/samtay/so-hs.svg?branch=master)](https://travis-ci.org/samtay/so-hs)

**Note:** I've since decided to rewrite this project in Rust
[here](https://github.com/samtay/so). The rewrite has way more features and is
more portable.

![terminal-gif](./doc/example.gif)

This implementation uses a basic prompt interface. The biggest drawback is that
the google parser is out-of-date, so all searches currently use the
StackOverflow API, which leaves much to be desired.

## install
First [get
stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/so-hs.git
cd so-hs
stack install so
```
