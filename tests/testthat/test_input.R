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
  testthat::expect_identical(runParallel(list(zoo, 
                                              bind(factorial, 1:3),
                                              bind(sys::exec_status, 
                                                   Sys.getpid(), 
                                                   F))),
                             list(function1=1L:3L,
                                  function2=c(1, 2, 6),
                                  function3=NA_integer_))
  testthat::expect_identical(runRace(list(function() Sys.sleep(7L),
                                          bind(sys::exec_status, 
                                               Sys.getpid(), 
                                               F))),
                             list(function1=NULL,
                                  function2=NA_integer_))
})