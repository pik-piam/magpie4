# extractWoodFuel

Extracts wood fuel demand from a MAgPIE GDX file at ISO country level
and converts from volumetric units (Mm³) to energy (PJ).

## Usage

``` r
extractWoodFuel(gdx, file = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

## Value

MAgPIE object with wood fuel demand at ISO level in PJ. Dimension 1: ISO
country, Dimension 2: year, Dimension 3: "woodfuel"

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- extractWoodFuel(gdx)
} # }
```
