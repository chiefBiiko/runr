# callback

testthat::context('callback')

moo <- function() 'moooo'
hoo <- function() list(17L:36L)
callback <- function(err, d) if (is.null(err)) d else stop(err, err$task)

testthat::test_that('callback has either error or data', {
  # without any error callback's return is list ...
  testthat::expect_type(runSeries(list(moo,
                                       hoo),
                                  callback),
                        'list')
  # either or principle - and error NULL
  testthat::expect_null(runRace(list(moo, 
                                     hoo),
                                function(err, d) list(err, d))[[1L]])
  # error object always has a $task property
  testthat::expect_type(runParallel(list(moo, 
                                         function() stop('color')),
                                    function(err, d) list(err, d))[[1L]]$task,
                        'character')
  # with 1st class error callback throws
  testthat::expect_error(runSeries(list(moo),
                                  callback))
  # with 2nd class error callback throws
  testthat::expect_error(runSeries(list(moo, 
                                        hoo,
                                        function() stop('horror')),
                                   callback))
})