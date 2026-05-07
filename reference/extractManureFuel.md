# extractManureFuel

Extracts manure used as direct combustion fuel from a MAgPIE GDX file at
ISO country level. Converts from nitrogen mass (Mt N) to energy (PJ)
using livestock-specific heating values and nitrogen contents from
Hoyos-Sebá et al. (2024).

## Usage

``` r
extractManureFuel(gdx, file = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

## Value

MAgPIE object with manure fuel at ISO level in PJ. Dimension 1: ISO
country, Dimension 2: year, Dimension 3: "manurefuel"

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- extractManureFuel(gdx)
} # }
```
