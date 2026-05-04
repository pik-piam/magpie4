# modelstat

Function to check if the library functions work with the newest magpie
version

## Usage

``` r
checkLibrary(gdx, level = NULL)
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

## Value

A list with three entries:

- `error` These functions could not be executed properly.

- `fine` Everything was fine with these functions.

## Details

This function simply tries to run all functions in the magpie library on
the provided gdx file.

## Author

Markus Bonsch

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- modelstat(gdx)
  } # }
```
