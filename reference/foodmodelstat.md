# foodmodelstat

MAgPIE food model statistics with information about convergence and
number of iterations

## Usage

``` r
foodmodelstat(gdx, file = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

## Value

A MAgPIE object containing number of iterations and convergence
information for each time step

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
x <- foodmodelstat(gdx)
} # }
```
