# extractProcessingWoodResidues

Extracts the potential of wood processing residues (sawmill byproducts)
from a MAgPIE GDX file at ISO country level and converts to energy (PJ).
Processing residues are computed from industrial roundwood demand using
a 30

## Usage

``` r
extractProcessingWoodResidues(gdx, file = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

## Value

MAgPIE object with potential wood processing residues at ISO level in
PJ. Dimension 1: ISO country, Dimension 2: year, Dimension 3:
"processingresidues"

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- extractProcessingWoodResidues(gdx)
} # }
```
