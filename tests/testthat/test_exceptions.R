# exceptions

testthat::context('exceptions')

moo <- function() 'moooo'
doo <- function() data.frame(a=0L:9L, 
                             b=10L:19L)

testthat::test_that('with exceptions no data return', {
  # without callback and 2nd class error run* return NULL
  testthat::expect_null(runSeries(list(moo,
                                       function() stop('throwing for fun'))))
  # without callback and 1st class error run* throw
  testthat::expect_error(runWaterfall(list(function() 1L,
                                           function() a + b + 1L)))
  testthat::expect_error(runParallel(list(doo)))
  # currently runr::bind does neither work with runr::runRace nor 
  # runr::runParallel - be aware
  testthat::expect_null(runParallel(list(doo, bind(factorial, 1:3),
                                         bind(jsonlite::fromJSON, 'https://api.github.com/users/chiefBiiko'))))
})