# powerParseRSON

powerParseRSON <- function(rson) {
  # Parses semi-parsed JSON2R data in a semi-safe manner.
  if (is.character(rson)) {  # beware if length(rson) > 1
    return(tryCatch(eval(parse(text=rson)),  # try casting
                    error=function(e) rson))
  } else {  # already cast
    return(rson)
  }
}