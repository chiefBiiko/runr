# runSeries

# Usage:
#   moo <- function() 'mooooooo'
#   runSeries(list(function() 1L, function() 2L, moo), 
#             function(d, err) if (is.null(err)) print(d) else print(err))

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')

runSeries <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb),
            if (is.function(cb)) length(formals(cb)) == 2L)
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series
  err <- NULL
  x <- withRestarts(  # restarts allow breaking an apply iterator
    lapply(1L:length(tasks), function(i) {
      tryCatch(tasks[[i]](),                                          # try
               error=function(e) {                                    # catch
                 err <<- sprintf('Error in %s: %s', games[i], geterrmessage())
               },
               finally=if (!is.null(err)) invokeRestart("stopLoop"))  # finally
    }),
    stopLoop=function() NULL  # if error break immediately and return NULL
  )
  # set names
  if (length(games) > length(x)) games <- games[1:length(x)]
  if (is.list(x)) names(x) <- games
  # returning
  return(if (is.function(cb)) cb(x, err) else x)
}
