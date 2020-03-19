# todo

### bugs
2. Fix --google flag & parser, way out of date now..

### features
2. Allow commands to switch between the two interfaces. (They both should have
1. Form URL and link to it at the end of showing questions / answers !!!!!
1. maybe --site defaults to Nothing (so long as --google is
   used) and we parse out any known URLs from google search and perform
   multiple stack exchange requests to each site (this is a nice to have and
   results in possibly too many API requests)
3. consider adding --verbose flag. this would be easy, but unclear how useful.
   one use case would be when google returns no results indicate that it may
   have detected non browser

### refactor for the love of god
0. Make a hasKey instance
0. Change `Question [] Text` to `Question t Text`s where possible (maybe even
   `Question t a` for those that really don't concern themsleves with Text.
   Reader AppState instances)
3. Why is NoResultError not being used in the general sense, and handled differently in each interface? Oi. What was I thinking? Use NonEmpty over [] and throw NoResultErrors when appropriate.
0. Consider removing the [-no] prefixed cli opts in favor of True/False with yaml boolean parser
3. Consider using container other than `[]` for questions/answers. I.e. map with key? NonEmpty?

### interface
1. Make a reflex-vty interface? Or commit to Brick..
1. `suspendAndResume` to view answer in editor of choice
6. in addition to vim h,j,k,l bindings for scrolling current viewport,
   shift+h,j,k,l for resizing sections?
7. or just a more thoughtful use of space in general.. maybe smaller viewports result in different views.

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
2. split `defaultOptions` and `sites` into `defaults.yml` and `sites.yml` ?
