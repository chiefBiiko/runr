runr
================

`runr` packs a set of higher order functions for running lists of functions in various modes.

:movie\_camera: *[runSeries](#runseries)*

:ocean: *[runWaterfall](#runwaterfall)*

:running: *[runRace](#runrace)*

:100: *[runParallel](#runparallel)*

------------------------------------------------------------------------

Get it
------

``` r
devtools::install_github('chiefBiiko/runr')
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
  if (!is.null(err)) stop(err, err$task)
  data
}
```

[`bounds`](https://github.com/chiefBiiko/bounds), a dependency of `runr`, has an export `bounds::bind` that allows binding parameters to functions. It takes a function and a variable sequence of parameters as inputs and returns a closure with the given parameters bound to it. Might come in handy at times.

------------------------------------------------------------------------

runSeries
---------

`runr::runSeries` runs its input tasks sequentially returning either a named list (on error `NULL`) or the value of a given callback.

``` r
# some named functions
moo <- function() 'moooo'
zoo <- function() 1L:3L

# run!
runr::runSeries(list(function() 1L, moo, zoo), callback)
```

    $function1
    [1] 1

    $function2
    [1] "moooo"

    $function3
    [1] 1 2 3

------------------------------------------------------------------------

runWaterfall
------------

`runr::runWaterfall` runs its input tasks sequentially, passing each task's return value to the next task, and returns either a named list (on error `NULL`) or the value of a given callback.

:ocean: All tasks except the first must have at least one parameter.

``` r
# chain/pipe consecutive returns
runr::runWaterfall(list(zoo,
                        base::factorial,  # reference names anyhow
                        bounds::bind(Reduce, function(a, b) a + b)))  # binding params
```

    $function1
    [1] 1 2 3

    $function2
    [1] 1 2 6

    $function3
    [1] 9

------------------------------------------------------------------------

runRace
-------

`runr::runRace` runs its input tasks parallel until the very first return of any of its tasks and returns either a named list (all `NULL` but one and on error `NULL`) or the value of a given callback.

``` r
# see how return is variable due to instable time lags between child launches
runr::runRace(list(function() {Sys.sleep(11L); '1st first'}, 
                   function() {Sys.sleep(10L); '2nd first'}), 
              callback)
```

    $function1
    NULL

    $function2
    [1] "2nd first"

------------------------------------------------------------------------

runParallel
-----------

`runr::runParallel` runs its input tasks parallel until all complete and returns either a named list (on error `NULL`) or the value of a given callback.

``` r
# callback
hireme <- function(err, d) {
  if (!is.null(err)) stop(err, err$task)  # check n go
  sprintf('dev: @chiefBiiko | hireable: %s%s | %s',
          as.character(d$function1$hireable), 
          d$function2, d$function3)
}

# see ya!
runr::runParallel(list(bounds::bind(jsonlite::fromJSON, 
                                    'https://api.github.com/users/chiefBiiko'), 
                       bounds::bind(strrep, '!', 10L),
                       function() gsub('[^3<]', '', '<kreuzberg36original>')), 
                  hireme)
```

    [1] "dev: @chiefBiiko | hireable: TRUE!!!!!!!!!! | <3"
