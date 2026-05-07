# extractCropResidues2ndBE

Extracts crop residues available for 2nd generation bioenergy from a
MAgPIE GDX file at ISO country level. Applies soil cover constraints
(minimum 30% soil cover retained, Lutz et al. 2019) and a collection
fraction to estimate sustainably harvestable residue biomass, converted
to energy (PJ).

## Usage

``` r
extractCropResidues2ndBE(
  gdx,
  file = NULL,
  collectionFraction = 0.25,
  minDensityForExtraction = 0.1
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- collectionFraction:

  fraction of available residues that can be physically collected
  (default: 0.25)

- minDensityForExtraction:

  minimum residue density (tDM/ha) below which extraction is not
  economic (default: 0.1)

## Value

MAgPIE object with crop residue potential for 2nd gen bioenergy at ISO
level in PJ. Dimension 1: ISO country, Dimension 2: year, Dimension 3:
residue types (res_cereals, res_fibrous, res_nonfibrous)

## References

Lutz et al. (2019), GMD,
<https://gmd.copernicus.org/articles/12/2419/2019/>

## See also

[`ResidueBiomass`](ResidueBiomass.md), [`ResidueUsage`](ResidueUsage.md)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- extractCropResidues2ndBE(gdx)
} # }
```
