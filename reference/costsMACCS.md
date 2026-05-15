# costsMACCS

reads costs entering the objective function from a MAgPIE gdx file

## Usage

``` r
costsMACCS(gdx, file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

## Value

MAgPIE object containing mitigation costs \[million US\$17\]

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- costsMACCS(gdx)
  } # }
```
