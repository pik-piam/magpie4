# reportProcessingWoodResidues

Reports the potential of wood processing residues (sawmill byproducts)
from a MAgPIE GDX file at ISO country level and converts to energy (PJ).
Processing residues are computed from industrial roundwood demand using
a 30

## Usage

``` r
reportProcessingWoodResidues(gdx, file = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

## Value

MAgPIE object with potential wood processing residues at ISO level in
PJ.

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- reportProcessingWoodResidues(gdx)
} # }
```
