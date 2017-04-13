# runSeries

# intended usage:
#   moo <- function() 'mooooooo'
#   runSeries(list(function() 1L, function() 2L, moo), function(d) print(d))

runSeries <- function(tasks=list(NULL), cb=NULL) {
  stopifnot(all(sapply(tasks, function(t) is.function(t))),
            is.null(cb) || is.function(cb))
  # setup
  mcall <- gsub('\\s+', ' ',  # get unevaluated inputs
                paste0(deparse(match.call()), collapse=''), perl=T)
  # extract tasks functions only as length 1 chr vector
  funcs <- if (is.null(cb)) {  # case no cb
    sub('^.+list\\((.*)\\)\\)$', '\\1', mcall, perl=T)
  } else {                     # case cb
    sub('^.*list\\((.*)(\\),\\scb.+)|(\\)\\))$', '\\1', mcall, perl=T)
  }
  # split funcs on comma and space
  split <- strsplit(funcs, ', ', fixed=T)[[1]]
  # substitute unnamed functions with 'anonymous'
  aames <- sub('function.*', 'anonymous', split, perl=T)
  # if anonymous suffix is index
  games <- sapply(1:length(aames), function(i) {
    if (aames[i] == 'anonymous') {
      paste0('anonymous', as.character(i))
    } else {
      aames[i]
    }
  })
  # call series
  x <- lapply(tasks, function(t) t())
  # set names
  names(x) <- games
  # returning
  return(if (is.function(cb)) cb(x) else x)
}
