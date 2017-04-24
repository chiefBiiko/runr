# runWaterfall

#' Run a list of functions as waterfall
#'
#' \code{runWaterfall} runs its input tasks sequentially, passing each task's 
#' return value to the next task, and returns either a named list 
#' (on error \code{NULL}) or the value of a given callback.
#'
#' @param tasks List of functions (anonymous and named) \strong{required}.
#' @param cb Anonymous or named function with signature 
#' \code{cb(error, data)} \strong{optional}.
#' @return If \code{cb} is \code{NULL} the tasks' return values are returned 
#' in a named list (on error \code{NULL}). If \code{cb} is a function it is 
#' called upon completion of all tasks and gets passed an error value 
#' (default \code{NULL}) as first parameter and a named list of the tasks' 
#' return values (on error \code{NULL}) as second parameter.
#' 
#' @details All tasks except the first must have at least one parameter. 
#' If an error is encountered while calling the tasks without a 
#' callback \code{runWaterfall} immediately stops execution and returns 
#' \code{NULL}. If an error is encountered and a callback is defined 
#' \code{runWaterfall} immediately stops execution and calls the callback with 
#' the \code{data} parameter set to \code{NULL} and the \code{error} parameter 
#' set to the encountered error. Thus, the callback will always have only one 
#' non-\code{NULL} argument. Within the callback simply check for an error 
#' with \code{is.null(error)}. If the \code{error} object is not \code{NULL} 
#' it has a property \code{$task} indicating the function that failed.
#' 
#' @note \code{\link{bind}} allows binding parameters to a function.
#' 
#' @seealso \code{\link{runSeries}} \code{\link{runRace}} 
#' \code{\link{runParallel}} \code{\link{bind}}
#'  \url{https://github.com/feross/run-waterfall}
#' 
#' @examples
#' callback <- function(err, d) {
#'   if (is.null(err)) d else stop(err, err$task)
#' }
#' runWaterfall(list(function() 1L, 
#'                   function(a) a + 2L, 
#'                   function(a) a + 3L), 
#'              callback)
#'
#' @export
runWaterfall <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))), 
            length(tasks) >  1L, 
            is.null(cb) || is.function(cb))
  if (is.function(cb) && length(formals(cb)) != 2L) {
    stop('callback must have two parameters: 1st error, 2nd data')
  }
  # 
  if (!all(sapply(2L:length(tasks), function(i) {
    #length(formals(tasks[[i]])) || length(args(tasks[[i]]))  >= 1L
    if (typeof(tasks[[i]]) == 'builtin') {
      length(args(tasks[[i]]))  >= 1L
    } else if (typeof(tasks[[i]]) == 'closure') {
      length(formals(tasks[[i]])) >= 1L
    }
  }))) {
    stop('All tasks except the first must have at least one parameter')
  }
  # setup
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # call series as waterfall
  err <- NULL
  x <- list()  # accumulator and return object
  withRestarts(  # restarts allow breaking an apply iterator
    lapply(1L:length(tasks), function(i) {
      tryCatch(x[[games[i]]] <<- if (i == 1L) {                         # try
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
  return(if (is.function(cb)) cb(err, x) else x)
}
