# Seed

Calculates MAgPIE demand for Seed out of a gdx file

## Usage

``` r
Seed(gdx, level = "reg", attributes = "dm")
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm").
  Can also be a vector.

## Value

demand as MAgPIE object (Unit depends on attributes)

## Details

Demand definitions are equivalent to FAO CBS categories

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- demand(level="regglo", products="kcr")
  } # }
```
