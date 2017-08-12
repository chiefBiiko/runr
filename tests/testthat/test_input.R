# input

testthat::context('input')

testthat::test_that('any function can be used', {

  # setup
  moo <- function() 'moooo'
  zoo <- function() 1L:3L

  # hopefully runs without errors
  testthat::expect_identical(runSeries(list(bounds::bind(sum, 0L, 1L),
                                            moo)),
                             list(task1=1L,
                                  task2='moooo'))

  # binding
  testthat::expect_identical(runWaterfall(list(zoo,
                                               factorial,
                                               bounds::bind(Reduce, function(a, b) a + b))),
                             list(task1=1L:3L,
                                  task2=c(1, 2, 6),
                                  task3=9))

  # referencing
  testthat::expect_identical(runWaterfall(list(zoo,
                                               stats::median)),
                             list(zoo=1L:3L,
                                  `stats::median`=2L))

  # parallel binding
  testthat::expect_identical(runParallel(list(zoo,
                                              bounds::bind(factorial, 1:3),
                                              bounds::bind(rev, 1L:3L))),
                             list(task1=1L:3L,
                                  task2=c(1, 2, 6),
                                  task3=c(3L, 2L, 1L)))

  # parallel race binding
  testthat::expect_identical(runRace(list(function() Sys.sleep(10L),
                                          bounds::bind(trimws, '  fraud '))),
                             list(task1=NULL,
                                  task2='fraud'))
})
