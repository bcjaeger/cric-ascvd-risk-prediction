## Load your packages, e.g. library(drake).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(the_plan,
             lock_envir = FALSE,
             seed = 32987,
             prework = {

               rspec = round_spec() %>%
                 round_half_even() %>%
                 round_using_magnitude(
                   digits = c(2, 2, 1, 0),
                   breaks = c(1, 10, 100, Inf)
                 )

               names(rspec) <- paste('table.glue', names(rspec), sep = '.')

               options(rspec)

             })
