# tradedPrimariesBilateral

Converts trade flows into primary product equivalents by tracing
secondary and livestock products back to their primary inputs. Supports
both net trade flows and bilateral trade matrices. Trade flows are
decomposed into three pathways: (1) direct primary trade, (2) primaries
embodied in secondary products, and (3) primaries needed as feed for
livestock.

## Usage

``` r
tradedPrimariesBilateral(
  gdx,
  file = NULL,
  bilateral = TRUE,
  convFactor = "exporter",
  kastner = TRUE,
  level = "reg",
  disaggLivestock = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- bilateral:

  Logical. If TRUE, uses bilateral trade matrix (ov21_trade) if
  available. If FALSE, uses net trade flows. Default is TRUE.

- convFactor:

  Character. When using bilateral trade, determines whether processing
  shares and feed baskets are taken from "exporter" or "importer"
  region. Default is "exporter" (footprint allocation to producing
  region).

- kastner:

  Logical. If TRUE and bilateral=TRUE, applies Kastner et al. 2011
  adjustment to bilateral trade matrix. Default is TRUE.

- level:

  Regional aggregation level ("reg", "glo", "regglo", or custom).
  Default is "reg".

- disaggLivestock:

  Logical. If TRUE, the feed pathway retains the livestock product
  dimension so that feed crop demands can be traced back to specific
  animal products. Output dim 3 will be `kli_product.kve_product` for
  the feed pathway instead of the aggregated `feed.kve_product`. The
  `prim` and `secd` pathways are unchanged (`pathway.kve_product`).
  Default is FALSE.

## Value

MAgPIE object with primary product trade equivalents in dry matter
(tDM). For bilateral trade: dimensions are (exporter.importer, year,
pathway.product) For net trade: dimensions are (region, year,
pathway.product) When disaggLivestock=TRUE: feed items have dimensions
kli_product.kve_product instead of feed.kve_product; prim/secd items are
unchanged.

## Details

When convFactor="exporter", the exporting region's processing pathways
and feed baskets are used, which is appropriate for production-based
footprint accounting. When convFactor="importer", the importing region's
factors are used, which maintains backward compatibility with the
original tradedPrimaries function.

For bilateral trade, the output includes full origin-destination detail.
For net trade, the output is aggregated by region.

When `disaggLivestock=TRUE`, callers can assign embodied resources
either to the feed crop (collapse dim 3.1, i.e. the livestock dimension)
or to the animal product (collapse dim 3.2, i.e. the crop dimension).

## Author

David M Chen, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  # Bilateral trade with exporter's factors (production-based accounting)
  x <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "exporter")
  
  # Bilateral trade with importer's factors (consumption-based accounting)
  x <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "importer")
  
  # Net trade (backward compatible with tradedPrimaries)
  x <- tradedPrimariesBilateral(gdx, bilateral = FALSE)
} # }
```
