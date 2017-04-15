# runWaterfall

# Usage:
#   runWaterfall(list(function() 1L, function(a) a + 2L, function(a) a + 3L), 
#                function(d, err) print(d))

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')

runWaterfall <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb),
            if (is.function(cb)) length(formals(cb)) == 2L)
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series as waterfall
  err <- NULL
  x <- list()  # return object
  withRestarts(  # restarts allow breaking an apply iterator
    lapply(1L:length(games), function(i) {
      tryCatch(x[games[i]] <<- if (i == 1L) {                         # try
                 tasks[[i]]()  # first task does not have predecessor
               } else {
                 tasks[[i]](x[[i - 1L]])  # all others do
               },
               error=function(e) err <<- geterrmessage(),             # catch
               finally=if (!is.null(err)) invokeRestart("stopLoop"))  # finally
    }),
    stopLoop=function() x <<- NULL  # if error break and set x to NULL
  )
  # returning
  return(if (is.function(cb)) cb(x, err) else x)
}
