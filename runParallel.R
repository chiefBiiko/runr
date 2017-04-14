# runParallel

# intended usage:
#   runParallel(list(function(i=1L) while (i < 1e6L) i <- i + 1L, 
#                    function() {Sys.sleep(10L); return('parapara!')}), 
#               function(d) print(d))

lapply(list('sys', 'jsonlite'), function(p) {
  if (!p %in% .packages(T)) install.packages(p)
})

source('https://github.com/chiefBiiko/countMatch/raw/master/countMatch.R')

runParallel <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  # setup
  on.exit({
    lapply(PID, function(pid) tools::pskill(pid))
    unlink('runParallel', recursive=T)
  })
  dir.create('runParallel')  # root for all tasks
  lapply(1L:length(tasks), function(i) {  # each task gets an own dir
    dir.create(file.path('runParallel', as.character(i)))
  })
  mcall <- gsub('\\s+', ' ',  # get unevaluated inputs
                paste0(deparse(match.call()), collapse=''), perl=T)
  # extract tasks functions only as length 1 chr vector
  funcs <- if (is.null(cb)) {  # case no cb
    sub('^.+list\\((.*)\\)\\)$', '\\1', mcall, perl=T)
  } else {                     # case cb
    sub('^.*list\\((.*)(\\),\\scb.+)|(\\)\\))$', '\\1', mcall, perl=T)
  }
  # split funcs on comma and space
  split <- strsplit(funcs, ', ', fixed=T)[[1L]]
  # substitute unnamed functions with 'anonymous'
  aames <- sub('^function.+$', 'anonymous', split, perl=T)
  # if anonymous suffix is index
  games <- sapply(1L:length(aames), function(i) {
    if (aames[i] == 'anonymous') {
      paste0('anonymous', as.character(i))
    } else {
      aames[i]
    }
  })
  # make filenames
  FLNMS_R <- lapply(1L:length(games), function(i) {
    file.path('runParallel', 
              as.character(i), 
              paste0('xp.', games[i], '.R'))
  })
  FLNMS_JSON <- lapply(FLNMS_R, function(n) {
    sub('R$', 'json', n, perl=T)
  })
  # prepare input tasks
  xp.tasks <- lapply(1L:length(games), function(i) {
    paste0('sink(file=\'',
           FLNMS_JSON[[i]],
           '\')',
           '\n',
           'jsonlite::toJSON(c((', 
           paste0(deparse(tasks[[i]]), sep='\n', collapse=''), 
           ')(), \'runParallel_END\'))',
           '\n',
           'sink()')
  })
  # further preparation
  PID <- list()  # memory for PIDs of tasks
  lapply(1L:length(games), function(i) {
    # export prepared tasks to their designated directory
    cat(xp.tasks[[i]], file=FLNMS_R[[i]])
    # make a json log for each xp.task
    cat('', file=FLNMS_JSON[[i]])
    # start child processes and record their pids
    PID[games[i]] <<- sys::exec_background('Rscript', FLNMS_R[[i]], F, F)
  })
  # enter blocking loop till all tasks are done
  status <- lapply(PID, function(p) F)  # task status completed: T/F
  x <- lapply(status, function(s) NULL)  # return object
  i <- 1L
  repeat {  # preferring repeat with a counter over while
    # check if current task completed
    if (countMatch(FLNMS_JSON[[i]], 'runParallel_END', perl=F, fixed=T) > 0L) {
      # read in return value
      x[games[i]] <- jsonlite::fromJSON(FLNMS_JSON[[i]])[1L]
      # mark current task as completed
      status[games[i]] <- T
    }
    # check if all tasks have completed
    if (all(unlist(status))) break
    i <- i + 1L  # increment
    if (i > length(games)) i <- 1L  # rewind
  }
  # exit
  # substitute EOF error
  x <- lapply(x, function(v) if (v == 'runParallel_END') NULL else v)
  return(if (is.function(cb)) cb(x) else x)
}
