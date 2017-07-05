# so

# todo

### backend
0. actually conduit (or some other lazy method) might be good for this.
loading questions + answers for 50+ questions is a lot of text to load into memory.
1. google + stackoverflow api + json parser
3. Include `&num=100` in params of google query (100 == limiter)
4. remember to filter by `is_answered=true` or filter out unanswered after query
5. ref: `https://api.stackexchange.com/docs/types/question`
7. filters are finicky but work -- remember to make filter unsafe to not
worry about printing weird chars
8. add a --create-filters sub command to update xdga data with named filter
9. apparently i need to accept gzip?
10. template haskell or makefile to replace `@@STACKEXCHANGE_API_KEY@@` with my own, but
allow it to be overridable with envvar `$STACKEXCHANGE_API_KEY`.

### interface
0. maybe start this thing off with StateT ???
1. default to --google but allow --no-google
2. allow `--site|-s` option which defaults to 'stackoverflow'
  need a hardcoded map of (shortname, URL), URL used in site:URL of google search,
  and shortname used in stackexchange API
2.5. -- OR perhaps --site defaults to Nothing (so long as --google is used)
and we parse out any known URLs from google search and perform multiple stack exchange requests
to each site ??? (this is a nice to have and results in possibly too many API requests)
3. allow --print-sites to print all available sites and exit
10.  allow --count or some other limiter in google search
3. allow -l|--lucky to just straight print top voted answer
4. xdga config to set defaults, i.e. deault --no-google, default --lucky, etc.
2. brick, haskeline, both, or either?
3. whats the layout resulting from (2)? 3 columns like ranger?
4. vim bindings hjkl for traversing content
5. optional buffer viewer in configuration, e.g.: native, vim, emacs, less, more;
  quitting brings you back into so
