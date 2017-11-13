#' @keywords internal
isNonEmptyListOfInts <- function(x) {
  return(
    is.list(x) && length(x) != 0L && 
    all(sapply(x, function(y) is.numeric(y) && y %% 1 == 0))
  )
}

#' @keywords internal
isNonEmptyListofFuncs <- function(x) {
  return(
    is.list(x) && length(x) != 0L && all(sapply(x, function(y) is.function(y)))
  )
}

#' @keywords internal
nameI <- function(name, seq) {
  stopifnot(is.character(name), length(name) == 1L, is.numeric(seq))
  return(lapply(as.list(seq), function(i) paste0(name, as.character(i))))
}

#' @keywords internal
getFuncNames <- function(tasks, cb) {  # returns the names of tasks only
  mcall <- gsub('\\s+', ' ',  # get unevaluated inputs
                paste0(deparse(match.call(definition=sys.function(sys.parent(n=1L)),
                                          call=sys.call(sys.parent(n=1L)),
                                          expand.dots=TRUE,
                                          envir=parent.frame(n=2L))), 
                       collapse=''), 
                perl=TRUE)
  # extract tasks functions only as length 1 chr vector
  funcs <- if (is.null(cb)) {  # case no cb
    sub('^.*tasks = list\\((.*)\\)\\)$', '\\1', mcall, perl=TRUE)
  } else {                     # case cb
    sub('^.*tasks = list\\((.*)(\\),\\scb.+)|(\\)\\))$', '\\1', mcall, 
        perl=TRUE)
  }
  # final names
  games <- if (grepl('function|bind\\(', funcs, perl=TRUE)) {  # anonymous
    paste0(rep('task', length(tasks)), as.character(1L:length(tasks)))
  } else {  # case named only
    # split funcs on comma and space
    strsplit(funcs, ', (?![^()]*+\\))', perl=TRUE)[[1L]]
  }
  # returning name vector
  return(games)
}

#' @keywords internal
killEmAll <- function(PIDS) {
  stopifnot(isNonEmptyListOfInts(PIDS))
  killed <- sapply(PIDS, tools::pskill)
  while (!all(killed)) killed <- sapply(PIDS, tools::pskill)
}