# return

testthat::context('return')

testthat::test_that('return data is valid', {

  # setup
  moo <- function() 'moooo'
  doo <- function() data.frame(a=0L:9L, 
                               b=10L:19L)
  
  # return is named list
  testthat::expect_identical(runWaterfall(list(function() 111L,
                                               function(a) a + 2L)),
                             list(function1=111L,
                                  function2=113L))
  
  # data.frame's base type is list
  testthat::expect_type(runSeries(list(function() 111L,
                                       doo))$function2,
                        'list')
  
  # returns from children
  testthat::expect_identical(runParallel(list(function() F,
                                              function() NULL)),
                             list(function1=FALSE, 
                                  function2=NULL))
})