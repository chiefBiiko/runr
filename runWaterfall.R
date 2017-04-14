# runWaterfall

# intended usage:
#   runWaterfall(list(function() 1L, function(a) a + 2L, function(a) a + 3L), 
#                function(d) print(d))

runWaterfall <- function(tasks=list(NULL), cb=NULL) {
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
  # call series as waterfall
  x <- list()  # memory of return values
  lapply(1L:length(games), function(i) {
    if (i == 1L) {
      x[games[i]] <<- tasks[[i]]()
    } else {
      x[games[i]] <<- tasks[[i]](x[[i - 1L]])
    }
  })
  # returning
  return(if (is.function(cb)) cb(x) else x)
}
