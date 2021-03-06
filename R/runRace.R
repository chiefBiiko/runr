# runRace

#' Run a list of functions as race
#'
#' \code{runRace} runs its input tasks parallel until the very first return of
#' any of its tasks and returns either a named list (all \code{NULL} but one and
#' on error \code{NULL}) or the value of a given callback.
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
#' callback \code{runRace} immediately stops execution and returns
#' \code{NULL}. If an error is encountered and a callback is defined
#' \code{runRace} immediately stops execution and calls the callback with
#' the \code{data} parameter set to \code{NULL} and the \code{error} parameter
#' set to the encountered error. Thus, the callback will always have only one
#' non-\code{NULL} argument. Within the callback simply check for an error
#' with \code{is.null(error)}. If the \code{error} object is not \code{NULL}
#' it has a property \code{$task} indicating the function that failed.
#'
#' @seealso \code{\link{runSeries}} \code{\link{runWaterfall}}
#' \code{\link{runParallel}}
#'
#' @examples
#' \dontrun{
#' callback <- function(err, d) {
#'   if (is.null(err)) d else stop(err, err$task)
#' }
#' runRace(list(function() {Sys.sleep(11L); return('first first')},
#'              function() {Sys.sleep(10L); return('second first')}),
#'         callback)
#' }
#'
#' @export
runRace <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(isNonEmptyListofFuncs(tasks), is.null(cb) || is.function(cb))
  if (is.function(cb) && length(formals(cb)) != 2L)
    stop('callback must have two parameters: 1st error, 2nd data')
  # input function names
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # io setup
  if (!dir.exists('.runr'))
    dir.create('.runr')              # temp dir for slave scripts
  else
    unlink(file.path('.runr', '*'))  # clear old stuff
  # filenames
  FLNMS_R <- file.path('.runr', paste0('xp.', games, '.R'))
  MMNMS_LOG <- nameI('LOG', seq_along(FLNMS_R))
  MMNMS_OUT <- nameI('OUT', seq_along(FLNMS_R))
  MMNMS_BND <- nameI('BND', seq_along(FLNMS_R))
  # execution script template for children
  TMPL <- paste0('%s\n',  # for function object
                 '%s\n',  # for bound data
                 'RTN <- NULL\n',
                 'runr_END <- \'runr_EOF\'\n',
                 'sharedata::clone_environment(\'CLONE\', envir=.GlobalEnv)\n',
                 'list(\n',
                 'tryCatch(\n',
                 'assign(\'RTN\', (FUN)(), envir=.GlobalEnv),\n',
                 'error=function(e) {\n',
                 'assign(\'runr_END\',',
                 'geterrmessage(), envir=.GlobalEnv)\n',
                 'assign(\'RTN\', e, envir=.GlobalEnv)',
                 '}\n',
                 '),\n',
                 'sharedata::share_object(RTN, \'%s\')\n',
                 ')\n',
                 'sharedata::share_object(runr_END, \'%s\')')
  # clone parent's global environment data... sharedata::...
  sharedata::share_environment('CLONE', envir=.GlobalEnv)
  # further preparation
  PIDS <- list()  # memory for PIDSs of tasks
  xp.tasks <- vector('list', length(tasks))
  for (i in seq_along(tasks)) {
    # conditionally transfer bound environments
    if (bounds::isBound(tasks[[i]]))  # save bound environments
      sharedata::share_object(environment(tasks[[i]]), MMNMS_BND[[i]])
    # prepare input tasks
    xp.tasks[[i]] <- sprintf(
      TMPL,
      paste0('FUN <- ', paste0(deparse(tasks[[i]]), sep='\n', collapse='')),
      ifelse(bounds::isBound(tasks[[i]]), 
             paste0('environment(FUN) <- sharedata::clone_object(\'', 
                    MMNMS_BND[[i]], '\')'),
             ''),
      MMNMS_OUT[[i]],
      MMNMS_LOG[[i]]
    )
    # export prepared tasks
    cat(xp.tasks[[i]], file=FLNMS_R[[i]])
  }
  # start child processes non-blocking and record their pids
  for (i in seq_along(tasks)) {
    PIDS[[games[i]]] <- sys::exec_background(
      cmd='R', 
      args=c('--vanilla', '--slave', '-f', FLNMS_R[[i]]), 
      std_out=FALSE, std_err=FALSE
    )
  }
  # enter blocking loop till all tasks are done
  err <- NULL
  status <- lapply(PIDS, function(p) FALSE)  # task status completed: T/F
  x <- lapply(status, function(s) NULL)  # return object
  i <- 1L
  repeat {  # block
    # check if error occurred
    if (!status[[games[i]]] &&
        tryCatch(sharedata::clone_object(MMNMS_LOG[[i]]) != 'runr_EOF',
                 error=function(e) FALSE)) {
      x <- NULL  # set data to NULL
     Sys.sleep(.5)  # wait 4 OS to commit
      # read in error
     err <- sharedata::clone_object(MMNMS_OUT[[i]])
      # poll <- 30
      # while (poll > 0) {
      #   if (tryCatch(err <- sharedata::clone_object(MMNMS_OUT[[i]]),
      #                error=function(e) FALSE) != FALSE) {
      #     break
      #   }
      #   Sys.sleep(.1)
      #   poll <- poll - 1L
      # }
      err$task <- games[i]  # add info
      break  # early exit
    }
    # check if current task completed
    if (!status[[games[i]]] &&
        tryCatch(sharedata::clone_object(MMNMS_LOG[[i]]) == 'runr_EOF',
                 error=function(e) FALSE)) {
      Sys.sleep(.5)  # wait 4 OS to commit
      # read in return value
      RTN <- sharedata::clone_object(MMNMS_OUT[[i]])
      # poll <- 30
      # while (poll > 0) {
      #   if (tryCatch(RTN <- sharedata::clone_object(MMNMS_OUT[[i]]),
      #                error=function(e) FALSE) != FALSE) {
      #     break
      #   }
      #   Sys.sleep(.1)
      #   poll <- poll - 1L
      # }
      # not assigning NULL to prevent deleting named list item
      if (!is.null(RTN)) x[[games[i]]] <- RTN
      status[[games[i]]] <- TRUE  # mark current task as completed
    }
    # check if any tasks completed
    if (any(unlist(status))) break  # been time
    i <- i + 1L  # increment
    if (i > length(tasks)) i <- 1L  # rewind
  }
  # clean up
  killEmAll(PIDS)
  lapply(list(MMNMS_LOG, MMNMS_OUT, MMNMS_BND), sharedata::unshare)
  unlink('.runr', recursive=TRUE)
  # done
  return(if (is.function(cb)) cb(err, x) else x)
}
