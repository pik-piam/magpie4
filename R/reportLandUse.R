#' @title reportLandUse
#' @description reports land-use
#'
#' @export
#'
#' @param gdx GDX file
#' @param level The aggregation level to be used ("regglo" by default)
#' @return land-use as MAgPIE object (million ha)
#' @author Florian Humpenoeder, Kristine Karstens, Isabelle Weindl
#' @importFrom magclass getRegions
#' @examples
#'
#'   \dontrun{
#'     x <- reportLandUse(gdx)
#'   }
#'
#' @section Total land cover variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover | million ha | Total land cover
#' Resources\|Land Cover\|Agricultural land | million ha | Agricultural land including cropland and pastures
#'
#' @section Cropland variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|+\|Cropland | million ha | Arable land, i.e. land in bioenergy crop, food, and feed/fodder crops, permanent crops as well as other arable land (physical area)
#' Resources\|Land Cover\|Cropland\|+\|Croparea | million ha | Physical cropland area used for crop production
#' Resources\|Land Cover\|Cropland\|+\|Fallow | million ha | Fallow cropland
#' Resources\|Land Cover\|Cropland\|+\|Tree Cover | million ha | Trees on cropland for agroforestry
#'
#' @section Pasture and urban variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|+\|Pastures and Rangelands | million ha | Pasture and range land based on FAO definition of permanent meadows and pastures
#' Resources\|Land Cover\|+\|Urban Area | million ha | Built-up land associated with human settlements
#'
#' @section Other natural land variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|+\|Other Land | million ha | Non-forest natural land including primary non-forest, restored and recovered natural land
#' Resources\|Land Cover\|Other Land\|Initial | million ha | Primary non-forest natural land without clearly visible indications of human activities
#' Resources\|Land Cover\|Other Land\|Recovered | million ha | Recovered natural land due to the abandonment of agricultural or forestry land without intention for nature/biodiversity conservation
#' Resources\|Land Cover\|Other Land\|Restored | million ha | Intentionally restored natural land for the purpose of nature and/or biodiversity conservation
#'
#' @section Forest variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|+\|Forest | million ha | Sum of primary, secondary and planted forest (FAO definition)
#' Resources\|Land Cover\|Forest\|+\|Natural Forest | million ha | Naturally regenerated forest including primary and secondary forest
#' Resources\|Land Cover\|Forest\|Natural Forest\|+\|Primary Forest | million ha | Naturally regenerated forest of native tree species where there are no clearly visible indications of human activities (FAO definition)
#' Resources\|Land Cover\|Forest\|Natural Forest\|+\|Secondary Forest | million ha | Forest predominantly composed of trees established through natural regeneration excluding primary forest (based on FAO definition)
#' Resources\|Land Cover\|Forest\|Natural Forest\|Secondary Forest\|Young | million ha | Young secondary forest
#' Resources\|Land Cover\|Forest\|Natural Forest\|Secondary Forest\|Mature | million ha | Mature secondary forest
#' Resources\|Land Cover\|Forest\|+\|Planted Forest | million ha | Forest predominantly composed of trees established through planting and/or deliberate seeding (FAO definition)
#' Resources\|Land Cover\|Forest\|Planted Forest\|+\|Timber | million ha | Planted forest established to meet timber demand: intensively managed plantations, one or two species, even-aged, regular spacing (FAO Plantation Forest)
#' Resources\|Land Cover\|Forest\|Planted Forest\|+\|CO2-price AR | million ha | Afforestation/reforestation established in response to the CO2 price for carbon sequestration
#' Resources\|Land Cover\|Forest\|Planted Forest\|CO2-price AR\|+\|Plantation | million ha | CO2-price AR grown as a monoculture and/or with non-native species (FAO Plantation Forest); non-zero only when s32_aff_plantation=1
#' Resources\|Land Cover\|Forest\|Planted Forest\|CO2-price AR\|+\|Natural | million ha | CO2-price AR grown with native tree species resembling natural vegetation (FAO Other Planted Forest); non-zero only when s32_aff_plantation=0
#' Resources\|Land Cover\|Forest\|Planted Forest\|+\|NPI_NDC AR | million ha | Afforestation/reforestation established to meet national policies and NDC commitments (native species; FAO Other Planted Forest)
#' Resources\|Land Cover\|Forest\|Planted Forest\|+\|Other Planted | million ha | Planted forest present at model initialisation; the non-plantation component of FAO "Other Planted Forest"
#' @md


reportLandUse <- function(gdx, level = "regglo") {

  ### main land types
  #read in regional data
  landData <- land(gdx, level = level, types = NULL, subcategories = c("crop", "forestry"), sum = FALSE)
  landData <- setNames(landData, gsub("indc", "ndc", getNames(landData)))
  landData <- landData[, , "other", invert = TRUE]
  otherLandData <- OtherLand(gdx, level = level)
  landData <- mbind(otherLandData, landData)

  # "forestry_other_planted" is an optional forestry sub-pool. It is only present in gdx
  # files that carry the dedicated other-planted forestry sub-pool;
  # on all other gdx files the forestry pool is split into {aff, ndc, plant} only. Detecting
  # it once via intersect() keeps this report backward-compatible: when the pool is absent,
  # otherPlantedPool is character(0), so it drops out of the c()-selections in the forest
  # aggregates below and no additional "Other Planted" reporting line is created.
  otherPlantedPool <- intersect("forestry_other_planted", getNames(landData, dim = 1))

  secdforest <- gdxAggregate(gdx,
                             madrat::toolAggregate(readGDX(gdx, "ov35_secdforest", select = list(type = "level")),
                                                   readGDX(gdx, "ac_to_bii_class_secd"),
                                                   from = "ac", to = "bii_class_secd",
                                                   dim = 3.1),
                             to = level, absolute = TRUE)

  # aggregate and rename
  millionha <- " (million ha)"
  outputParts <- list(
    list("Resources|Land Cover (million ha)",
         dimSums(landData, dim = 3)),
    list(paste0("Resources|Land Cover|+|Cropland", millionha),
         dimSums(landData[, , c("crop_area", "crop_fallow", "crop_treecover")], dim = 3)),
    list(paste0("Resources|Land Cover|Cropland|+|", reportingnames("crop_area"), millionha),
         landData[, , "crop_area"]),
    list(paste0("Resources|Land Cover|Cropland|+|", reportingnames("crop_fallow"), millionha),
         landData[, , "crop_fallow"]),
    list(paste0("Resources|Land Cover|Cropland|+|", reportingnames("crop_treecover"), millionha),
         landData[, , "crop_treecover"]),
    list(paste0("Resources|Land Cover|+|", reportingnames("past"), millionha),
         landData[, , "past"]),
    list(paste0("Resources|Land Cover|+|", reportingnames("urban"), millionha),
         landData[, , "urban"]),
    list(paste0("Resources|Land Cover|+|", reportingnames("other"), millionha),
         dimSums(landData[, , c("other_initial", "other_recovered", "other_restored")], dim = 3)),
    list(paste0("Resources|Land Cover|", reportingnames("other"), "|Initial", millionha),
         landData[, , "other_initial"]),
    list(paste0("Resources|Land Cover|", reportingnames("other"), "|Recovered", millionha),
         landData[, , "other_recovered"]),
    list(paste0("Resources|Land Cover|", reportingnames("other"), "|Restored", millionha),
         landData[, , "other_restored"]),
    list(paste0("Resources|Land Cover|+|", reportingnames("forest"), millionha),
         dimSums(landData[, , c("primforest", "secdforest", "forestry_aff", "forestry_ndc",
                                "forestry_plant", otherPlantedPool)], dim = 3)),
    list(paste0("Resources|Land Cover|Forest|+|", reportingnames("natrforest"), millionha),
         dimSums(landData[, , c("primforest", "secdforest")], dim = 3)),
    list(paste0("Resources|Land Cover|Forest|Natural Forest|+|", reportingnames("primforest"), millionha),
         dimSums(landData[, , "primforest"], dim = 3)),
    list(paste0("Resources|Land Cover|Forest|Natural Forest|+|", reportingnames("secdforest"), millionha),
         dimSums(landData[, , "secdforest"], dim = 3)),
    if (is.magpie(secdforest)) {
      list(paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), "|Young", millionha),
           secdforest[, , "secd_young"])
    },
    if (is.magpie(secdforest)) {
      list(paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), "|Mature", millionha),
           secdforest[, , "secd_mature"])
    },
    list(paste0("Resources|Land Cover|Forest|+|", reportingnames("forestry"), millionha),
         dimSums(landData[, , c("forestry_aff", "forestry_ndc", "forestry_plant", otherPlantedPool)], dim = 3))
  )

  # Planted forest by establishment origin (purpose-primary, consistent with reportEmissions'
  # Regrowth|CO2-price AR|{Plantation, Natural}). CO2-price AR (forestry_aff) is a single line;
  # the s32_aff_plantation switch only decides whether it is grown as a plantation or as native
  # (natural-vegetation-like) forest, shown as the CO2-price AR sub-split.
  s32AffPlantation <- readGDX(gdx, "s32_aff_plantation")
  zero       <- new.magpie(getItems(landData, 1.1), getYears(landData), NULL, fill = 0, sets = getSets(landData))
  affTotal   <- dimSums(landData[, , "forestry_aff"], dim = 3)
  affPlant   <- if (s32AffPlantation == 1) affTotal else zero
  affNatural <- if (s32AffPlantation == 0) affTotal else zero
  outputParts <- append(outputParts, list(
    list("Resources|Land Cover|Forest|Planted Forest|+|Timber (million ha)",
         dimSums(landData[, , "forestry_plant"], dim = 3)),
    list("Resources|Land Cover|Forest|Planted Forest|+|CO2-price AR (million ha)",
         affTotal),
    list("Resources|Land Cover|Forest|Planted Forest|CO2-price AR|+|Plantation (million ha)",
         affPlant),
    list("Resources|Land Cover|Forest|Planted Forest|CO2-price AR|+|Natural (million ha)",
         affNatural),
    list("Resources|Land Cover|Forest|Planted Forest|+|NPI_NDC AR (million ha)",
         dimSums(landData[, , "forestry_ndc"], dim = 3))
  ))

  # FRA "Other Planted Forest" - a third sibling of Plantations and Natural under Planted
  # Forest, reported only when the dedicated other-planted forest pool exists in the gdx
  # (see otherPlantedPool above). Kept out of the Natural aggregate so that the Natural|+|
  # children (CO2-price AR, NPI_NDC AR) still sum to Natural; instead it sums into Planted
  # Forest directly alongside Plantations and Natural.
  if (length(otherPlantedPool) > 0) {
    outputParts <- append(outputParts, list(
      list("Resources|Land Cover|Forest|Planted Forest|+|Other Planted (million ha)",
           dimSums(landData[, , otherPlantedPool], dim = 3))
    ))
  }

  outputParts <- append(outputParts, list(
    list(paste0("Resources|Land Cover|Agricultural land", millionha),
         dimSums(landData[, , c("crop_area", "crop_fallow", "crop_treecover", "past")], dim = 3))
  ))

  outputParts <- Filter(Negate(is.null), outputParts) |>
    lapply(function(part) {
      return(setNames(part[[2]], part[[1]]))
    })

  result <- do.call(mbind, outputParts)

  return(result)
}
