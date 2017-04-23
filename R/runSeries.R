# runSeries

#' Run a list of functions as series
#'
#' \code{runSeries} runs its input tasks sequentially returning either a named 
#' list (on error \code{NULL}) or the value of a given callback.
#'
#' @param tasks List of function objects (anonymous and named) 
#' \strong{required}.
#' @param cb Anonymous or named function object with signature 
#' \code{cb(error, data)} \strong{optional}.
#' @return If \code{cb} is \code{NULL} the tasks' return values are returned 
#' in a named list (on error \code{NULL}). If \code{cb} is a function it is 
#' called upon completion of all tasks and gets passed an error value 
#' (default \code{NULL}) as first parameter and a named list of the tasks' 
#' return values (on error \code{NULL}) as second parameter.
#' 
#' @details If an error is encountered while calling the series without a 
#' callback \code{runSeries} immediately stops execution and returns 
#' \code{NULL}. If an error is encountered and a callback is defined 
#' \code{runSeries} immediately stops execution and calls the callback with 
#' the \code{data} parameter set to \code{NULL} and the \code{error} parameter 
#' set to the encountered error. Thus, the callback will always have only one 
#' non-\code{NULL} parameter. Within the callback simply check for an error 
#' with \code{is.null(error)}. If the \code{error} object is not \code{NULL} 
#' it has a property \code{$task} indicating the function that failed.
#' 
#' @note \code{\link{bind}} allows binding parameters to a function.
#' 
#' @seealso \code{\link{runWaterfall}} \code{\link{runRace}} 
#' \code{\link{runParallel}} \code{\link{bind}} 
#' \url{https://github.com/feross/run-series}
#' 
#' @examples
#' moo <- function() 'mooooooo'
#' callback <- function(err, d) {
#'   if (is.null(err)) d else stop(err, err$task)
#' }
#' runSeries(list(function() 1L, 
#'                function() 2L, 
#'                moo), 
#'           callback)
#'
#' @export
runSeries <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))), 
            length(tasks) >  1L, 
            is.null(cb) || is.function(cb))
  if (is.function(cb) && length(formals(cb)) != 2L) {
    stop('callback must have two parameters: 1st error, 2nd data')
  }
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series
  err <- NULL
  x <- withRestarts(  # restarts allow breaking an apply iterator
    lapply(1L:length(tasks), function(i) {
      tryCatch(tasks[[i]](),                                          # try
               error=function(e) {                                    # catch
                 e$task <- games[i]  # new property on error obj
                 err <<- e
               },
               finally=if (!is.null(err)) invokeRestart("stopLoop"))  # finally
    }),
    stopLoop=function() NULL  # if error break immediately and return NULL
  )
  # set names
  if (length(games) > length(x)) games <- games[1L:length(x)]
  if (is.list(x)) names(x) <- games
  # returning
  return(if (is.function(cb)) cb(err, x) else x)
}
