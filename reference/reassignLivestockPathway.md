# reassignLivestockPathway

Collapse every livestock (`kli`) product's prim/secd/feed pathway split
into a single `feed` (Livestock) pathway, leaving all non-livestock
products untouched. The Kastner prim/secd/feed pathway split in
[`embodiedResourceKastner`](embodiedResourceKastner.md) is keyed on each
product's demand share, so a livestock product's OWN footprint
(enteric/manure emissions, herding labour, drinking water) lands in the
prim/secd pathway of the `kli` product. To show the FULL livestock
footprint in the Livestock pathway, this moves each `kli` product's
whole footprint (all pathways) into `feed`. Non-`kli` feed crops/pasture
keep their pathway, so they still count as Livestock via the `feed`
pathway.

Works on both layouts produced by
[`embodiedResourceKastner`](embodiedResourceKastner.md): the bilateral
object (dim 3 = `pathway.product`) and the regional accounting object
(dim 3 = `accounting.pathway.product`); the pathway and product
sub-dimensions are located by name/position, so the object must carry a
sub-dimension named `pathway` with the product as the last sub-dimension
of dim 3. The operation is a no-op when no `kli` product is present
(e.g. the land resource, whose livestock land sits in the feed pathway
of crops) and is idempotent (re-applying it changes nothing). Each
product's total (summed over pathway) is conserved.

## Usage

``` r
reassignLivestockPathway(x, kli = NULL, gdx = NULL)
```

## Arguments

- x:

  MAgPIE object with a `pathway` sub-dimension in dim 3 and the product
  as the last sub-dimension of dim 3 (bilateral or regional output of
  [`embodiedResourceKastner`](embodiedResourceKastner.md) and its
  `embodied*Kastner` wrappers).

- kli:

  character vector of livestock product names (the `kli` set).
  Alternatively supply `gdx` to read it.

- gdx:

  optional GDX file to read the `kli` set from when `kli` is not given.

## Value

MAgPIE object of the same layout as `x`, with all `kli` products'
footprint moved to the `feed` pathway.

## See also

[`embodiedResourceKastner`](embodiedResourceKastner.md),
[`footprints`](footprints.md)

## Author

David M Chen
