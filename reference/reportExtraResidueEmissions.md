# reportExtraResidueEmissions.R

reads residue biomass from a MAgPIE gdx file and multiplies DM by
emission factors from GFED

## Usage

``` r
reportExtraResidueEmissions(gdx, level = "reg")
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global)

## Value

emissions as MAgPIE object (unit: Mt X/yr, plus cumulative Mt X/yr)

## Residue burning emission variables

|                                                                                |           |                                     |
|--------------------------------------------------------------------------------|-----------|-------------------------------------|
| Name                                                                           | Unit      | Meta                                |
| Emissions\|CO2\|Land\|Biomass Burning\|+\|Burning of Crop Residues             | Mt CO2/yr | CO2 emissions from residue burning  |
| Emissions\|CO\|Land\|Biomass Burning\|+\|Burning of Crop Residues              | Mt CO/yr  | CO emissions from residue burning   |
| Emissions\|BC\|Land\|Biomass Burning\|+\|Burning of Crop Residues              | Mt BC/yr  | Black carbon from residue burning   |
| Emissions\|VOC\|Land\|Biomass Burning\|+\|Burning of Crop Residues             | Mt VOC/yr | VOC emissions from residue burning  |
| Emissions\|CO2\|Land\|Cumulative\|Biomass Burning\|+\|Burning of Crop Residues | Mt CO2    | Cumulative CO2 from residue burning |

## Author

Michael Crawford

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportExtraResidueEmissions(gdx, level = "glo")
  } # }
```
