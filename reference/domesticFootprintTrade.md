# domesticFootprintTrade

Calculates embodied land in trade using the domestic technology
assumption.

## Usage

``` r
domesticFootprintTrade(
  gdx,
  file = NULL,
  level = "reg",
  type = "net-trade",
  productsAggr = TRUE,
  maxIntensityRatio = 5
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg", "glo", "regglo"

- type:

  "exports", "imports", or "net-trade" (default)

- productsAggr:

  Aggregation of primary products; default is TRUE

- maxIntensityRatio:

  Maximum allowed ratio of regional to historical (1995) land intensity.
  Regional land intensities exceeding this factor times the 1995 value
  are capped. This same value is used for filling NAs. Use NULL for no
  capping. Default is 5 (caps at 5× 1995 values).

## Value

MAgPIE object with embodied land in trade (Mha)

## Details

The domestic technology assumption values all traded goods using each
region's own processing shares, feed baskets, and land intensity
(yields). For exports this represents the actual land footprint. For
imports this is a counterfactual: how much land would the region need if
it produced its imports domestically?

Trade flows are decomposed into primary product equivalents via three
pathways: (1) direct primary trade, (2) primaries embodied in secondary
products, (3) primaries needed as livestock feed. Land intensity
(ha/tDM) is then applied per primary product.

Regional land intensities are filled and capped using historical (1995)
FAO yields as reference. This ensures comparability across different
model runs while accounting for regional differences in agricultural
potential.

This is a consistent approach with net trade data (no bilateral flows).
With bilateral trade data, different basis for land intensity could be
applied.

## See also

[`tradedPrimaries`](tradedPrimaries.md)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- domesticFootprintTrade(gdx, type = "net-trade")
} # }
```
