# so [![Build Status](https://travis-ci.org/samtay/so.svg?branch=master)](https://travis-ci.org/samtay/so)


This isn't done yet! If you're reading this, feel free to bookmark the repo but
come back in like a month for the first release. I'm just pushing to GitHub so
that it's easier to work across two computers.

# todo

1. Decide whether new queries can be run from prompt/brick interfaces, or are
   they purely for viewing results and then exiting?
2. Stop flip flopping on keeping [Question] in AppState. (1) might depend on this.
3. Also related, possibly split `runApp` right at the start into brick/prompt executions,
   in case they handle querying (and more specifically, synch/asynch requests) differently.

### stack exchange
1. filters are finicky but work -- remember to make filter unsafe to not worry
   about printing weird chars
2. add a `--update-filters` sub command to update xdga data with named filter.
   in fact, maybe this ships without hardcoded filters and on first run,
   creates new filter and stores in xdga data.
5. handle and write tests for error responses:
   https://api.stackexchange.com/docs/types/error easy to test by setting bogus
   api key. need to catch `StatusCodeException` with code 400

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
7. can set up themes named in configuration and then keep attrmap within app
   state
3. Consider parallelizing the request, keeping an MVar within SO state, and
   just pushing to it from requests, then issuing a custom event and updating
   brick state with new QAs!  ^^ THIS IS THE RIGHT WAY TO DO THIS:
   https://nanonaren.wordpress.com/2014/07/23/example-parsing-hackage-with-tagsoup/

### config
1. xdga data to save `filter` param
2. split `defaultOptions` and `sites` into `defaults.yml` and `sites.yml`

### general
1. in the future, maybe --site defaults to Nothing (so long as --google is
   used) and we parse out any known URLs from google search and perform
   multiple stack exchange requests to each site (this is a nice to have and
   results in possibly too many API requests)
3. consider adding --verbose flag. this would be easy, but unclear how useful.
   one use case would be when google returns no results indicate that it may
   have detected non browser
