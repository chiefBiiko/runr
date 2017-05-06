# runr utils

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
    paste0(rep('function', length(tasks)), as.character(1L:length(tasks)))
  } else {  # case named only
    # split funcs on comma and space
    strsplit(funcs, ', (?![^()]*+\\))', perl=TRUE)[[1L]]
  }
  # returning name vector
  return(games)
}

#' @keywords internal
countMatch <- function(text=NULL, pattern=NULL, 
                       ignore.case=FALSE, 
                       perl=TRUE, 
                       fixed=FALSE, 
                       useBytes=FALSE) {
  # Counts matches with pattern in text.
  # @param {chr} text Length 1 chr vector, can be a chr literal or a filename
  # @param {chr} pattern Regular expression, can be perl-like
  # @return {int} Number of matches of pattern in text
  stopifnot(is.character(text), is.character(pattern),
            length(text) == 1L, length(pattern) == 1L)
  # get input
  if (file.exists(text)) {
    CON <- file(text)
    on.exit(close(CON))
    input <- paste0(readLines(CON, warn=F), collapse='')
  } else if (nchar(text) > 0L) {
    input <- text
  } else { stop('invalid input!') }
  # return count
  return(lengths(regmatches(input, gregexpr(pattern, input, 
                                            ignore.case, 
                                            perl, 
                                            fixed, 
                                            useBytes))))
}