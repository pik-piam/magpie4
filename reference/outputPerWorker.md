# outputPerWorker

returns output per worker in crop+livestock production

## Usage

``` r
outputPerWorker(gdx, level = "reg", file = NULL)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation to report employment ("reg", "glo", "regglo", or
  custom region aggregation)

- file:

  a file name the output should be written to using write.magpie

## Value

output per worker as magpie object

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- outputPerWorker(gdx)
} # }
```
