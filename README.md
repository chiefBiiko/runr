runr
================

[![Build Status](https://travis-ci.org/chiefbiiko/runr.svg?branch=master)](https://travis-ci.org/chiefbiiko/runr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chiefbiiko/runr?branch=master&svg=true)](https://ci.appveyor.com/project/chiefbiiko/runr) [![Project Status: WIP â Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

------------------------------------------------------------------------

`runr` packs a set of higher order functions for running lists of functions in various modes.

------------------------------------------------------------------------

:movie\_camera: *[runSeries](#runseries)*

:ocean: *[runWaterfall](#runwaterfall)*

:running: *[runRace](#runrace)*

:100: *[runParallel](#runparallel)*

------------------------------------------------------------------------

Get it
------

``` r
devtools::install_github('chiefbiiko/runr')
```

------------------------------------------------------------------------

API
---

`runr::run*(tasks = list(NULL), cb = NULL)`

-   `tasks` List of functions **required**
-   `cb` Callback with signature `cb(error, data)` **optional**

Values for the `tasks` or `cb` parameter can be defined anonymous or referenced to via a valid function name. If a callback is defined it will always have exactly one non-`NULL` argument only. Without errors during task execution the `data` argument of the callback is a named list. In case of errors during task execution the `error` argument of the callback is an ordinary error object with *one* additional property `$task` which indicates the function that threw.

``` r
# callback skeleton - must have exactly two parameters
callback <- function(err, data) {
  if (!is.null(err)) stop(err, err$task)  # check n go
  data
}
```

[`bounds`](https://github.com/chiefbiiko/bounds), a dependency of `runr`, has an export `bounds::bind` that allows binding parameters to functions. It takes a function and a variable sequence of parameters as inputs and returns a closure with the given parameters bound to it. Might come in handy at times.

------------------------------------------------------------------------

runSeries
---------

`runr::runSeries` runs its input tasks sequentially returning either a named list (on error `NULL`) or the value of a given callback.

``` r
# a named function
moo <- function() 'moooo'

# run as series
runr::runSeries(list(Sys.getpid, Sys.time, moo), callback)
```

    $Sys.getpid
    [1] 158584

    $Sys.time
    [1] "2017-11-13 17:59:36 CET"

    $moo
    [1] "moooo"

------------------------------------------------------------------------

runWaterfall
------------

`runr::runWaterfall` runs its input tasks sequentially, passing each task's return value to the next task, and returns either a named list (on error `NULL`) or the value of a given callback.

:ocean: All tasks except the first must have at least one parameter.

``` r
# another named function
zoo <- function() 1L:3L

# bind and create a named closure
reduceSum <- bounds::bind(Reduce, function(a, b) a + b)

# chain/pipe consecutive returns
runr::runWaterfall(list(zoo, factorial, reduceSum), callback)
```

    $zoo
    [1] 1 2 3

    $factorial
    [1] 1 2 6

    $reduceSum
    [1] 9

------------------------------------------------------------------------

runRace
-------

`runr::runRace` runs its input tasks parallel, blocks until the very first return of any of its tasks and returns either a named list (all `NULL` but one; on error `NULL`) or the value of a given callback.

``` r
# two resembling workers
dlHuckPDF <- bounds::bind(utils::download.file, 
                          'http://contentserver.adobe.com/store/books/HuckFinn.pdf',
                          'huck_finn.pdf')
dlHuckTXT <- bounds::bind(utils::download.file, 
                          'http://www.textfiles.com/etext/AUTHORS/TWAIN/huck_finn',
                          'huck_finn.txt')

# run a race
runr::runRace(list(dlHuckPDF, dlHuckTXT), callback)
```

    $dlHuckPDF
    NULL

    $dlHuckTXT
    [1] 0

------------------------------------------------------------------------

runParallel
-----------

`runr::runParallel` runs its input tasks parallel, blocks until all complete and returns either a named list (on error `NULL`) or the value of a given callback.

``` r
# some stoopid workers
d <- bounds::bind(jsonlite::fromJSON, 'https://api.github.com/users/chiefbiiko')
o <- bounds::bind(base::sub, 
                  '^.*(3).*$', '\\1', paste0(installed.packages(), collapse=''))

# a new callback
cb <- function(err, data) {
  if (!is.null(err)) stop(err, err$task)
  sprintf('@%s | %s | <%s',
          data$d$login, grep('hi', names(data$d), value=TRUE), data$o)
}

# see ya!
runr::runParallel(list(d, o), cb)
```

    [1] "@chiefbiiko | hireable | <3"

------------------------------------------------------------------------

TODO
----

runRace, runParallel

-   decrease timeout when reading children's OUT back into main R parent

------------------------------------------------------------------------

License
-------

[MIT](./LICENSE)
