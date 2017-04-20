# errors

testthat::context('errors')

moo <- function() 'moooo'
doo <- function() data.frame(a=0L:9L, 
                             b=10L:19L)

testthat::test_that('on error some error return', {
  # case error encountered
  testthat::expect_null(runSeries(list(moo,
                                        function() stop('throwing for fun'))))
  testthat::expect_error(runWaterfall(list(function() 1L,
                                           function(a, b) a + b + 1L)))
  testthat::expect_error(runParallel(list(doo)))
})