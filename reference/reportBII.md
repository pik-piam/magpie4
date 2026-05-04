# reportBII

reports biodiversity intactness index

## Usage

``` r
reportBII(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

Biodiversity intactness index as MAgPIE object

## Biodiversity Intactness Index variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Biodiversity\|BII | unitless | Terrestrial biodiversity measured with Biodiversity Intactness Index (BII). The BII summarises the change in ecological communities in response to human pressures. It is an estimated percentage of the original number of species that remain and their abundance in any given area. |
| Biodiversity\|Cropland Landscapes BII | unitless | Terrestrial biodiversity in landscapes containing cropland measured with Biodiversity Intactness Index (BII) |
| Biodiversity\|BII in 30x30 Landscapes | unitless | Terrestrial biodiversity in 30 by 30 conservation target landscapes measured with Biodiversity Intactness Index (BII) |
| Biodiversity\|Biodiversity Hotspot BII | unitless | Terrestrial biodiversity in biodiversity hotspot landscapes measured with Biodiversity Intactness Index (BII) |
| Biodiversity\|Biodiversity Hotspot and Intact Forest Landscapes BII | unitless | Terrestrial biodiversity in biodiversity hotspot and intact forest landscapes measured with Biodiversity Intactness Index (BII) |
| Biodiversity\|BII in areas outside Biodiversity Hotspots, Intact Forest & Cropland Landscapes | unitless | Terrestrial biodiversity in areas outside biodiversity hotspot, intact forest and cropland landscapes measured with Biodiversity Intactness Index (BII) |
| Biodiversity\|Key Biodiversity Area BII | unitless | Terrestrial biodiversity in key biodiversity areas measured with Biodiversity Intactness Index (BII) |

## Author

Patrick v. Jeetze, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportBII(gdx)
} # }
```
