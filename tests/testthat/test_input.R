# input

testthat::context('input')

moo <- function() 'moooo'
zoo <- function() 1L:3L

testthat::test_that('any function can be used', {
  # hopefully runs without errors
  testthat::expect_identical(runSeries(list(bind(sum, 0L, 1L),
                                            moo)),
                             list(function1=1L,
                                  function2='moooo'))
  testthat::expect_identical(runWaterfall(list(zoo,
                                               factorial,
                                               bind(Reduce, function(a, b) a + b))),
                             list(function1=1L:3L,
                                  function2=c(1, 2, 6),
                                  function3=9))
  testthat::expect_identical(runWaterfall(list(zoo, 
                                               stats::median)),
                             list(zoo=1L:3L,
                                  `stats::median`=2L))
})