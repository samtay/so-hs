# so [![Build Status](https://travis-ci.org/samtay/so.svg?branch=master)](https://travis-ci.org/samtay/so)

**Note: this is under active development.** The prompt interface is fully
functional, but the more sophisticated brick interface is not finished.

# todo

0. Consider removing the [-no] prefixed cli opts in favor of True/False with yaml boolean parser
1. Form URL and link to it at the end of showing questions / answers !!!!!
3. Consider using container other than `[]` for questions/answers. 
4. Test --no-google flag
5. Write a .nix builder

### interface
0. j/k to scroll, Ctrl + j/k/l/h to move, and Ctrl+Shift + j/k/h/l to resize
   the four panes (e.g. visually moving the vertical and horizontal dividers)
1. maybe custom widget holder to maintain column ratios
2. extents might be useful for rendering previews efficiently
3. vty supports reverseVideo style (mappend with selected item - see ListDemo)
4. check if caching makes sense for expensive widgets (probably clear on
   resize)
5. `suspendAndResume` to view answer in editor of choice
6. in addition to vim h,j,k,l bindings for scrolling current viewport,
   shift+h,j,k,l for resizing sections
7. can set up themes named in configuration and then keep attrmap within app
   state -- HELL YEAH JT JUST ADDED THIS TO BRICK !!!!

### stack exchange
1. filters are finicky but work -- remember to make filter unsafe to not worry
   about printing weird chars
2. add a `--update-filters` sub command to update xdga data with named filter.
   in fact, maybe this ships without hardcoded filters and on first run,
   creates new filter and stores in xdga data.
5. handle and write tests for error responses:
   https://api.stackexchange.com/docs/types/error easy to test by setting bogus
   api key. need to catch `StatusCodeException` with code 400

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
