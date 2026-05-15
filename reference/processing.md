# processing

Calculates MAgPIE disaggregated processing out of a gdx file

## Usage

``` r
processing(gdx, level = "reg", indicator = "secondary_from_primary")
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

- indicator:

  process or secondary product output

## Value

processing as MAgPIE object (Unit depends on attributes)

## Details

Demand definitions are equivalent to FAO CBS categories

## Author

David Chen, Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- processing(gdx = gdx, level="regglo", products="kcr", indicator="primary_to_process")
  } # }
```
