# countMatch

countMatch <- function(text=NULL, pattern=NULL, 
                       ignore.case=F, perl=T, fixed=F, useBytes=F) {
  # Counts matches with pattern in text.
  # @param {chr} text Length 1 chr vector, can be a chr literal or a filename
  # @param {chr} pattern Regular expression, can be perl-like
  # @return {int} Number of matches of pattern in text
  stopifnot(is.character(text), is.character(pattern),
            length(text) == 1, length(pattern) == 1)
  # get input
  if (file.exists(text)) {
    CON <- file(text)
    on.exit(close(CON))
    input <- paste0(readLines(CON, warn=F), collapse='')
  } else if (nchar(text) > 0) {
    input <- text
  } else { stop('invalid input!') }
  # return count
  return(lengths(regmatches(input, gregexpr(pattern, input, 
                                            ignore.case, 
                                            perl, 
                                            fixed, 
                                            useBytes))))
}