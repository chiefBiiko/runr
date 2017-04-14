# runSeries

# Usage:
#   moo <- function() 'mooooooo'
#   runSeries(list(function() 1L, function() 2L, moo), function(d, e) print(d))

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')

runSeries <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series
  ERR <- NULL
  x <- lapply(tasks, function(task) {
    tryCatch(task(), 
             error=function(e) {
               ERR <<- e
               return(e)
             })
  })
  # set names
  if (length(games) > length(x)) games <- games[1:length(x)]
  names(x) <- games
  # returning
  return(if (is.function(cb)) cb(x, ERR) else x)
}
