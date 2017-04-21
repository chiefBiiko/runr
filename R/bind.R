# bind

#' Bind parameters to a function.
#'
#' \code{bind} returns a closure with given parameters bound to it.
#' 
#' @param func Function prototype object \strong{required}.
#' @param ... Unnamed or named parameters to bind \strong{required}.
#' 
#' @seealso \code{\link{runSeries}} \code{\link{runWaterfall}} 
#' \code{\link{runRace}} \code{\link{runParallel}}
#' 
#' @examples
#' boo <- function(a, b) a + b  # func with no defaults
#' runSeries(list(function() 0L, 
#'                bind(boo, 1L, 2L)))  # bind on the fly
#' 
#' @family runFunctions
#' @export
bind <- function(func, ...) {  # func proto
  stopifnot(!missing(func), !missing(...))
  bound <- list(...)  # static defaults
  function(...) {  # returning a closure
    new <- list(...)
    do.call(func, c(bound, new))
  }
}