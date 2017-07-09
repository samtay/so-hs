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

### config
0. xdga config: FIX allow user to set default site with shortcode instead of full info
1. xdga data to save `--filter` string
2. split `defaultOptions` and `sites` into `defaults.yml` and `sites.yml`

### cli
0. After too much late-night-can-barely-think programming, Cli module is kind
of a jumble of effectful functions all over the place. Replace all the parse/exec funcs
with Cli datatype that holds Options as well as flags like --print-sites. Then just
have a single exec that handles all effects.
1. Consider adding ansi-terminal for colors in prompting and error msgs (modeled like yaourt)
   -- this is added in the `ansi-colors` branch

### general
0. maybe start this thing off with StateT ??? or choose a new extension to
   learn and run with it?
  read: http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
1. in the future, maybe --site defaults to Nothing (so long as --google is
   used) and we parse out any known URLs from google search and perform
   multiple stack exchange requests to each site (this is a nice to have and
   results in possibly too many API requests)
