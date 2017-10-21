# so [![Build Status](https://travis-ci.org/samtay/so.svg?branch=master)](https://travis-ci.org/samtay/so)

**Note: this is under active development.** The prompt interface is fully
functional, but the more sophisticated brick interface is not finished. Also
the markdown is currently printed raw, but the finished product will parse this
and apply the styles possible within a terminal (e.g. bold, coloring code
sections, etc.)

# todo

0. Attach body text preview + parsed body datatype to Q/A types instead of raw stuff!!!
0. Trace evaluations to ensure md parsing happens lazily
1. Form URL and link to it at the end of showing questions / answers !!!!!
2. Consider removing `Either Error [Question]` and just throwing `Error` as an exception..
3. Consider using container other than `[]` for questions/answers. 
4. **Need** to lazily parse stackoverflow text into limited markdown doc types,
   then have renderers for said types in both brick and prompt (i.e. `code text`
   goes to cyan color, etc.)
4. Test --no-google flag
6. freeze dependency versions

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
0. Both interfaces will need parser for markdown. The brick adaptation might be
   useful for others.
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
8. reference: https://nanonaren.wordpress.com/2014/07/23/example-parsing-hackage-with-tagsoup/

### config
1. xdga data to save `filter` param
2. split `defaultOptions` and `sites` into `defaults.yml` and `sites.yml`

### future
1. maybe --site defaults to Nothing (so long as --google is
   used) and we parse out any known URLs from google search and perform
   multiple stack exchange requests to each site (this is a nice to have and
   results in possibly too many API requests)
3. consider adding --verbose flag. this would be easy, but unclear how useful.
   one use case would be when google returns no results indicate that it may
   have detected non browser
