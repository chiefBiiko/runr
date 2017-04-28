# runParallel

#' Run a list of functions parallel
#'
#' \code{runParallel} runs its input tasks parallel until all complete 
#' and returns either a named list (on error \code{NULL}) or the value 
#' of a given callback.
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
#' @details If an error is encountered while calling the tasks without a 
#' callback \code{runParallel} immediately stops execution and returns 
#' \code{NULL}. If an error is encountered and a callback is defined 
#' \code{runParallel} immediately stops execution and calls the callback with 
#' the \code{data} parameter set to \code{NULL} and the \code{error} parameter 
#' set to the encountered error. Thus, the callback will always have only one 
#' non-\code{NULL} argument. Within the callback simply check for an error 
#' with \code{is.null(error)}. If the \code{error} object is not \code{NULL} 
#' it has a property \code{$task} indicating the function that failed.
#' 
#' @note \code{\link{bind}} allows binding parameters to a function.
#' 
#' @seealso \code{\link{runSeries}} \code{\link{runWaterfall}} 
#' \code{\link{runRace}} \code{\link{bind}}
#' \url{https://github.com/feross/run-parallel}
#' 
#' @examples
#' callback <- function(err, d) {
#'   if (is.null(err)) d else stop(err, err$task)
#' }
#' runParallel(list(function(i=1L) while (i < 1e6L) i <- i + 1L, 
#'                  function() {Sys.sleep(7L); return('parapara!')}), 
#'             callback)
#'
#' @export
runParallel <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))), 
            length(tasks) >  1L, 
            is.null(cb) || is.function(cb))
  if (is.function(cb) && length(formals(cb)) != 2L) {
    stop('callback must have two parameters: 1st error, 2nd data')
  }
  # input function names
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # setup
  on.exit({  # clean up
    try(lapply(PID, function(pid) tools::pskill(pid)), silent=T)
    unlink('runParallel', recursive=T)
  })
  # io
  if (!dir.exists('runParallel')) {
    dir.create('runParallel')  # root for all tasks
  } else {
    unlink('runParallel/*')    # clear old stuff
  }
  # filenames
  FLNMS_R <- lapply(1L:length(games), function(i) {
    file.path('runParallel', paste0('xp.', games[i], '.R'))
  })
  FLNMS_LOG <- lapply(FLNMS_R, function(n) sub('R$', 'log', n, perl=T))
  FLNMS_RDS <- lapply(FLNMS_R, function(n) sub('R$', 'rds', n, perl=T))
  FLNMS_BND <- lapply(FLNMS_RDS, function(n) sub('xp', 'bound.xp', n, perl=T))
  # clone parent's global environment data
  save(list=ls(all.names=T, envir=.GlobalEnv), 
       file="runParallel/clone.RData", envir=.GlobalEnv)
  # further preparation
  PID <- list()  # memory for PIDs of tasks
  lapply(1L:length(tasks), function(i) {
    # conditionally transfer bound environments
    if (bounds::isBound(tasks[[i]])) {  # save bound environments
      saveRDS(environment(tasks[[i]]), FLNMS_BND[[i]]) 
    }
    # prepare input tasks
    xp.task <- sprintf(paste0('%s\n',  # for function object
                              '%s\n',  # for bound data
                              'RTN <- NULL\n', 
                              'runParallel_END <- \'runParallel_EOF\'\n',
                              'load(\'runParallel/clone.RData\')\n', 
                              'list(\n', 
                              'tryCatch(\n',
                              'assign(\'RTN\', (FUN)(), envir=.GlobalEnv),\n',  
                              'error=function(e) {\n',
                              'assign(\'runParallel_END\', ', 
                              'geterrmessage(), envir=.GlobalEnv)\n',
                              'assign(\'RTN\', e, envir=.GlobalEnv)\n',
                              '}\n',
                              '),\n',
                              'writeLines(runParallel_END, \'%s\')',
                              ')\n', 
                              'saveRDS(RTN, file=\'%s\')'), 
                       paste0('FUN <- ', 
                              paste0(deparse(tasks[[i]]), sep='\n', collapse='')),
                       if (bounds::isBound(tasks[[i]])) {
                         paste0('BOUND <- readRDS(\'', FLNMS_BND[[i]], '\')\n',
                                'environment(FUN) <- BOUND')
                       } else { '' },
                       FLNMS_LOG[[i]],
                       FLNMS_RDS[[i]])
    # export prepared tasks
    cat(xp.task, file=FLNMS_R[[i]])
    # make a log for each xp.task
    cat('', file=FLNMS_LOG[[i]])
    # start child processes non-blocking and record their pids
    PID[[games[i]]] <<- sys::exec_background('Rscript', FLNMS_R[[i]], F, F)
  })
  # enter blocking loop till all tasks are done
  err <- NULL
  status <- lapply(PID, function(p) F)  # task status completed: T/F
  x <- lapply(status, function(s) NULL)  # return object
  i <- 1L
  repeat {  # block
    # check if error occurred
    if (!status[[games[i]]] &&
        countMatch(FLNMS_LOG[[i]], '[^runParallel_EOF]') > 0L &&
        file.exists(FLNMS_RDS[[i]])) {
      x <- NULL  # set data to NULL
      Sys.sleep(1L)  # wait 4 OS to commit
      # read in error
      err <- readRDS(file=FLNMS_RDS[[i]])
      err$task <- games[i]  # add info
      break  # early exit
    }
    # check if current task completed
    if (!status[[games[i]]] &&
        countMatch(FLNMS_LOG[[i]], 'runParallel_EOF', perl=F, fixed=T) > 0L && 
        file.exists(FLNMS_RDS[[i]])) {
      # read in return value
      Sys.sleep(1L)  # wait 4 OS to commit
      RTN <- readRDS(file=FLNMS_RDS[[i]])
      # not assigning NULL to prevent deleting named list item
      if (!is.null(RTN)) x[[games[i]]] <- RTN
      status[[games[i]]] <- T  # mark current task as completed
    }
    # check if all tasks completed
    if (all(unlist(status))) break  # been time
    i <- i + 1L  # increment
    if (i > length(tasks)) i <- 1L  # rewind
  }
  # exit
  return(if (is.function(cb)) cb(err, x) else x)
}
