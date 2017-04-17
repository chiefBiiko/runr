# runParallel

# Usage:
#   runParallel(list(function(i=1L) while (i < 1e6L) i <- i + 1L, 
#                    function() {Sys.sleep(10L); return('parapara!')}), 
#               function(d, err) if (is.null(err)) d else stop(err))

if (!'sys' %in% .packages(T)) install.packages('sys')

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')
source('https://github.com/chiefBiiko/countMatch/raw/master/countMatch.R')

runParallel <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  if (is.function(cb) && length(formals(cb)) != 2L) {
    stop('callback must have two parameters: 1st data, 2nd error')
  }
  # input function names
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # setup
  on.exit({  # clean up
    unlink('runParallel', recursive=T)
    lapply(PID, function(pid) tools::pskill(pid))
  })
  # io
  if (!dir.exists('runParallel')) {
    dir.create('runParallel')  # root for all tasks
  } else {
    unlink('runParallel/*')    # clear old stuff
  }
  # clone parent's global environment data
  save(list=ls(all.names=T, envir=.GlobalEnv), 
       file="runParallel/clone.RData", envir=.GlobalEnv)
  # filenames
  FLNMS_R <- lapply(1L:length(games), function(i) {
    file.path('runParallel', paste0('xp.', games[i], '.R'))
  })
  FLNMS_LOG <- lapply(FLNMS_R, function(n) sub('R$', 'log', n, perl=T))
  FLNMS_RDS <- lapply(FLNMS_R, function(n) sub('R$', 'rds', n, perl=T))
  # further preparation
  PID <- list()  # memory for PIDs of tasks
  lapply(1L:length(tasks), function(i) {
    # prepare input tasks
    xp.task <- sprintf(paste0('TASK <- \'%s\'\n',
                              'RTN <- NULL\n', 
                              'runParallel_END <- \'runParallel_EOF\'\n',
                              'load(\'runParallel/clone.RData\')\n', 
                              'list(\n', 
                              'tryCatch(\n',
                              'assign(\'RTN\', (%s)(), envir=.GlobalEnv),\n',  
                              'error=function(e) {\n',
                              'assign(\'runParallel_END\', ', 
                              'paste0(\'Error in \', TASK, \' : \', ', 
                              'geterrmessage()), envir=.GlobalEnv)\n',
                              'assign(\'RTN\', e, envir=.GlobalEnv)\n',
                              '}\n',
                              '),\n',
                              'writeLines(runParallel_END, \'%s\')',
                              ')\n', 
                              'saveRDS(RTN, file=\'%s\')'), 
                       games[i],
                       paste0(deparse(tasks[[i]]), sep='\n', collapse=''),
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
      err$functionName <- games[i]  # add info
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
  return(if (is.function(cb)) cb(x, err) else x)
}
