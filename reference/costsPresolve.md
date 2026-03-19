# costsPresolve

reads presovle costs (i.e. without bioenergy demand) entering the
objective function from a MAgPIE gdx file

## Usage

``` r
costsPresolve(gdx, file = NULL, level = "reg")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

## Value

A MAgPIE object containing the goal function costs in presolve mode
\[million US\$17\]

## Details

Presolve is without bioenergy demand. Hence costs from a MAgPIE run with
bioenergy demand minus costs from presolve reflect costs that can be
attributed to bioenergy production

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- costsPresolve(gdx)
  } # }
```
