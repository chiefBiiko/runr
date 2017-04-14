# runSeries

# intended usage:
#   moo <- function() 'mooooooo'
#   runSeries(list(function() 1L, function() 2L, moo), function(d) print(d))

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')

runSeries <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series
  x <- lapply(tasks, function(t) t())
  # set names
  names(x) <- games
  # returning
  return(if (is.function(cb)) cb(x) else x)
}
