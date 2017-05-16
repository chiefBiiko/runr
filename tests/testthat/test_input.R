# input

testthat::context('input')

testthat::test_that('any function can be used', {

  # setup
  moo <- function() 'moooo'
  zoo <- function() 1L:3L
  
  # hopefully runs without errors
  testthat::expect_identical(runSeries(list(bounds::bind(sum, 0L, 1L),
                                            moo)),
                             list(function1=1L,
                                  function2='moooo'))
  
  # binding
  testthat::expect_identical(runWaterfall(list(zoo,
                                               factorial,
                                               bounds::bind(Reduce, function(a, b) a + b))),
                             list(function1=1L:3L,
                                  function2=c(1, 2, 6),
                                  function3=9))
  
  # referencing
  testthat::expect_identical(runWaterfall(list(zoo, 
                                               stats::median)),
                             list(zoo=1L:3L,
                                  `stats::median`=2L))
  
  # parallel binding
  testthat::expect_identical(runParallel(list(zoo, 
                                              bounds::bind(factorial, 1:3),
                                              bounds::bind(sys::exec_status, 
                                                           Sys.getpid(), 
                                                           F))),
                             list(function1=1L:3L,
                                  function2=c(1, 2, 6),
                                  function3=NA_integer_))
  
  # parallel race binding
  testthat::expect_identical(runRace(list(function() Sys.sleep(7L),
                                          bounds::bind(sys::exec_status, 
                                                       Sys.getpid(), 
                                                       F))),
                             list(function1=NULL,
                                  function2=NA_integer_))
})