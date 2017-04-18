# runWaterfall

# Usage:
#   runWaterfall(list(function() 1L, function(a) a + 2L, function(a) a + 3L), 
#                function(d, err) if (is.null(err)) d else stop(err, err$task))

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')

runWaterfall <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            length(tasks) >  1,
            is.null(cb) || is.function(cb))
  if (is.function(cb) && length(formals(cb)) != 2L) {
    stop('callback must have two parameters: 1st data, 2nd error')
  }
  # 
  if (!all(sapply(2L:length(tasks), function(i) {
    length(formals(tasks[[i]])) == 1
  }))) {
    stop('All tasks except the first must have exactly one parameter')
  }
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series as waterfall
  err <- NULL
  x <- list()  # accumulator and return object
  withRestarts(  # restarts allow breaking an apply iterator
    lapply(1L:length(tasks), function(i) {
      tryCatch(x[games[i]] <<- if (i == 1L) {                         # try
                 tasks[[i]]()  # first task does not have predecessor
               } else {
                 tasks[[i]](x[[i - 1L]])  # all others do
               },
               error=function(e) {                                    # catch
                 e$task <- games[i]  # new property on error obj
                 err <<- e
               }, 
               finally=if (!is.null(err)) invokeRestart("stopLoop"))  # finally
    }),
    stopLoop=function() x <<- NULL  # if error break and set x to NULL
  )
  # returning
  return(if (is.function(cb)) cb(x, err) else x)
}
