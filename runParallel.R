# runParallel

# Usage:
#   runParallel(list(function(i=1L) while (i < 1e6L) i <- i + 1L, 
#                    function() {Sys.sleep(10L); return('parapara!')}), 
#               function(d) print(d))

lapply(list('sys', 'jsonlite'), function(p) {
  if (!p %in% .packages(T)) install.packages(p)
})

source('https://github.com/chiefBiiko/runr/raw/master/getFuncNames.R')
source('https://github.com/chiefBiiko/countMatch/raw/master/countMatch.R')

runParallel <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  # setup
  on.exit({  # clean up
    unlink('runParallel', recursive=T)
    lapply(PID, function(pid) tools::pskill(pid))
  })
  # io
  dir.create('runParallel')  # root for all tasks
  # function names
  games <- getFuncNames(tasks, cb)  # returns the names of tasks only
  # filenames
  FLNMS_R <- lapply(1L:length(games), function(i) {
    file.path('runParallel', paste0('xp.', games[i], '.R'))
  })
  FLNMS_JSON <- lapply(FLNMS_R, function(n) sub('R$', 'json', n, perl=T))
  # further preparation
  PID <- list()  # memory for PIDs of tasks
  lapply(1L:length(games), function(i) {
    # prepare input tasks
    xp.task <- sprintf(paste0('sink(file=\'%s\')\n', 
                              'jsonlite::toJSON(c((%s)(), ', 
                              '\'runParallel_END\'))\n', 
                               'sink()'), 
                       FLNMS_JSON[[i]], 
                       paste0(deparse(tasks[[i]]), sep='\n', collapse=''))
    # export prepared tasks to their designated directory
    cat(xp.task, file=FLNMS_R[[i]])
    # make a json log for each xp.task
    cat('', file=FLNMS_JSON[[i]])
    # start child processes non-blocking and record their pids
    PID[games[i]] <<- sys::exec_background('Rscript', FLNMS_R[[i]], F, F)
  })
  # enter blocking loop till all tasks are done
  status <- lapply(PID, function(p) F)  # task status completed: T/F
  x <- lapply(status, function(s) NULL)  # return object
  i <- 1L
  repeat {  # block
    # check if current task completed
    if (countMatch(FLNMS_JSON[[i]], 'runParallel_END', perl=F, fixed=T) > 0L) {
      # read in return value
      x[games[i]] <- jsonlite::fromJSON(FLNMS_JSON[[i]])[1L]
      # mark current task as completed
      status[games[i]] <- T
    }
    # check if all tasks completed
    if (all(unlist(status))) break  # been time
    i <- i + 1L  # increment
    if (i > length(games)) i <- 1L  # rewind
  }
  # exit
  # substitute EOF error
  x <- lapply(x, function(v) if (v == 'runParallel_END') NULL else v)
  return(if (is.function(cb)) cb(x) else x)
}
