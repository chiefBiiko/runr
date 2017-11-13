# exceptions

testthat::context('exceptions')

testthat::test_that('with exceptions no data return', {

  # setup
  moo <- function() 'moooo'
  doo <- function() data.frame(a=0L:9L, 
                               b=10L:19L)

  # without callback and 2nd class error run* return NULL
  testthat::expect_null(runSeries(list(moo,
                                       function() stop('throwing for fun'))))

  # without callback and 1st class error run* throw
  testthat::expect_error(runWaterfall(list(function() 1L,
                                           function() a + b + 1L)))
  # cb must have 2 parameters
  testthat::expect_error(runParallel(list(doo), function(err) {}))
})