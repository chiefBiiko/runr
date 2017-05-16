# naming

testthat::context('naming')

testthat::test_that('naming convention is robust', {

  # setup
  moo <- function() 'moooo'
  doo <- function() data.frame(a=0L:9L, 
                               b=10L:19L)

  # anonymous inputs only
  testthat::expect_named(runSeries(list(function() 0L, 
                                        function() NULL)),
                         c('function1', 'function2'))

  # mixed inputs
  testthat::expect_named(runWaterfall(list(moo, 
                                           function(a) paste0(a, 'la'))),
                         c('function1', 'function2'))

  # named inputs only
  testthat::expect_named(runParallel(list(moo, 
                                          doo)),
                         c('moo', 'doo'))

  # messed up anonymous inputs
  testthat::expect_named(runParallel(list(function(a, b) 
                                            list(a=1L, b=77L),
                                          function() list(a=0.5564, 
                                                          b=0.3443,
                                                          z=FALSE),
                                          function(x, y) {  # nonsense
                                            Sys.sleep(7L)
                                            return(0L)
                                          })),
                         c('function1', 'function2', 'function3'))
})

