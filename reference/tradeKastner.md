# tradeKastner

Implements Kastner et al. 2011 (DOI: 10.1016/j.ecolecon.2011.01.012) on
the MAgPIE bilateral trade matrix. Adjusts the trade matrix based on a
assumption of proportionality such that intermediate trade partners
(importing to re-export) are removed from the matrix.

## Usage

``` r
tradeKastner(gdx, trade, level = "reg", products = "kall", attributes = "dm")
```

## Arguments

- gdx:

  GDX file to read from (for production data)

- trade:

  Bilateral trade data (ov21_trade or other)

- level:

  Level of regional aggregation ("reg", "glo", "regglo", or custom
  mapping)

- products:

  Selection of products (e.g. "kall", "kcr", "kli")

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm")

## Value

MAgPIE object with bilateral trade adjusted using Kastner method.
Dimensions: (importer_exporter, year, product) Values represent apparent
consumption of bilateral trade (production + imports - exports)

## Author

David M Chen
