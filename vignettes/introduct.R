## Require Packages====

library(fars)
library(maps) # for map plot


## Included data====

list.files(system.file("extdata", package = "fars"))


## Package Functions====
### Function `make_filename`====

make_filename(year = '2013')


### Function `fars_read_years`====

fars_read_years(years = c(2014, 2015))


### Function `fars_summarize_years`====

plot(fars_summarize_years(2015))
fars_summarize_years(c(2015, 2014))


### Function `fars_map_state`====

fars_map_state(35, 2013)
