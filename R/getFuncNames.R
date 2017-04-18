# getFuncNames

getFuncNames <- function(tasks, cb) {  # returns the names of tasks only
  mcall <- gsub('\\s+', ' ',  # get unevaluated inputs
                paste0(deparse(match.call(definition=sys.function(sys.parent(n=1L)),
                                          call=sys.call(sys.parent(n=1L)),
                                          expand.dots=T,
                                          envir=parent.frame(n=2L))), 
                       collapse=''), 
                perl=T)
  # extract tasks functions only as length 1 chr vector
  funcs <- if (is.null(cb)) {  # case no cb
    sub('^.*tasks = list\\((.*)\\)\\)$', '\\1', mcall, perl=T)
  } else {                     # case cb
    sub('^.*tasks = list\\((.*)(\\),\\scb.+)|(\\)\\))$', '\\1', mcall, perl=T)
  }
  # final names
  games <- if (grepl('function', funcs, fixed=T)) {  # case includes anonymous
    paste0(rep('function', length(tasks)), as.character(1L:length(tasks)))
  } else {  # case named only
    # split funcs on comma and space
    strsplit(funcs, ', (?![^()]*+\\))', perl=T)[[1L]]
  }
  # returning name vector
  return(games)
}