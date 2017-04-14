# runWaterfall

# intended usage:
#   runWaterfall(list(function() 1L, function(a) a + 2L, function(a) a + 3L), 
#                function(d) print(d))

runWaterfall <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series as waterfall
  x <- list()  # memory of return values
  lapply(1L:length(games), function(i) {
    if (i == 1L) {
      x[games[i]] <<- tasks[[i]]()
    } else {
      x[games[i]] <<- tasks[[i]](x[[i - 1L]])
    }
  })
  # returning
  return(if (is.function(cb)) cb(x) else x)
}
