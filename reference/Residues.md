# Residues

reads various crop residue (carbon) outputs out of a MAgPIE gdx file

## Usage

``` r
Residues(
  gdx,
  level = "regglo",
  products = "kres",
  waterAggr = TRUE,
  output = "all"
)
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global)

- products:

  Selection of products (either "kcr" or "kres")

- waterAggr:

  Aggregate irrigated and non-irriagted production or not (boolean).

- output:

  Switch between different outputs: "biomass", "fieldBalance",
  "resDemand", all

## Value

production as MAgPIE object (unit depends on attributes)

## See also

[`ResidueBiomass`](ResidueBiomass.md)

## Author

Kristine Karstens, Michael Crawford

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- Residues(gdx)
  } # }
```
