# so [![Build Status](https://travis-ci.org/samtay/so.svg?branch=master)](https://travis-ci.org/samtay/so)


This isn't done yet! If you're reading this, feel free to bookmark the repo but
come back in like a month for the first release. I'm just pushing to GitHub so
that it's easier to work across two computers.

# todo

### google
0. Google scraper + parser
1. Include `&num=100` in params of google query (100 == limiter)

### stack exchange
0. actually conduit (or some other lazy method) might be good for this.
   loading questions + answers for 50+ questions is a lot of text to load into
   memory.
1. filters are finicky but work -- remember to make filter unsafe to not worry
   about printing weird chars
2. add a `--create-filters` sub command to update xdga data with named filter
3. apparently i need to accept gzip?
4. template haskell or makefile to replace `@@STACKEXCHANGE_API_KEY@@` with my
   own, but allow it to be overridable with envvar `$STACKEXCHANGE_API_KEY` and
   add to field to Config as well.

### cli
0. finish parsers in Cli.hs
1. might already be time to start some StateT

### interface
0. both sophisticated brick column interface and basic prompting (haskeline or
   plain)
1. maybe custom widget holder to maintain column ratios
2. extents might be useful for rendering previews efficiently
3. vty supports reverseVideo style (mappend with selected item - see ListDemo)
4. check if caching makes sense for expensive widgets (probably clear on
   resize)
5. `suspendAndResume` to view answer in editor of choice
6. in addition to vim h,j,k,l bindings for scrolling current viewport,
   shift+h,j,k,l for resizing sections
7. Note that optparse-applicative has `execParserPure` !

### config
0. xdga config to set defaults, i.e. deault `--no-google`, default `--lucky`,
   etc.
2. xdga config for buffer viewer, e.g.: native, vim, less, more
1. xdga data to save `--filter` string

### general
0. maybe start this thing off with StateT ??? or choose a new extension to
   learn and run with it?
  read: http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
1. in the future, maybe --site defaults to Nothing (so long as --google is
   used) and we parse out any known URLs from google search and perform
   multiple stack exchange requests to each site (this is a nice to have and
   results in possibly too many API requests)
