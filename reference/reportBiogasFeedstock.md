# reportBiogasFeedstock

Reports biogas feedstock from a MAgPIE GDX file at ISO country level.
Currently covers two sources: manure allocated to digesters and fodder
crops (placeholder, all zeros until MAgPIE tracks foddr/forage crop
allocation to biogas). Manure is converted from nitrogen mass (Mt N) to
energy (PJ) using livestock-specific heating values and nitrogen
contents from Hoyos-Sebá et al. (2024).

## Usage

``` r
reportBiogasFeedstock(gdx, file = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

## Value

MAgPIE object with biogas feedstock potential ("manure", "fodder") at
ISO level in PJ.

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- reportBiogasFeedstock(gdx)
} # }
```
