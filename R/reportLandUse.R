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
#' Resources\|Land Cover\|Forest\|Planted Forest\|+\|Plantations | million ha | Intensively managed planted forests with one or two species, even age class, and regular spacing (FAO definition)
#' Resources\|Land Cover\|Forest\|Planted Forest\|Plantations\|+\|Timber | million ha | Plantations for timber production
#' Resources\|Land Cover\|Forest\|Planted Forest\|Plantations\|+\|CO2-price AR | million ha | Reforestation and/or afforestation for carbon sequestration with non-native species and/or as monoculture plantation
#' Resources\|Land Cover\|Forest\|Planted Forest\|+\|Natural | million ha | Planted forest not classified as plantation forest
#' Resources\|Land Cover\|Forest\|Planted Forest\|Natural\|+\|CO2-price AR | million ha | Reforestation and/or afforestation for carbon sequestration with native tree species resembling natural vegetation
#' Resources\|Land Cover\|Forest\|Planted Forest\|Natural\|+\|NPI_NDC AR | million ha | Afforestation/reforestation under national policies and NDC commitments
#' @md


reportLandUse <- function(gdx, level = "regglo") {

  ### main land types
  #read in regional data
  landData <- land(gdx, level = level, types = NULL, subcategories = c("crop", "forestry"), sum = FALSE)
  landData <- setNames(landData, gsub("indc", "ndc", getNames(landData)))
  landData <- landData[, , "other", invert = TRUE]
  otherLandData <- OtherLand(gdx, level = level)
  landData <- mbind(otherLandData, landData)
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
         dimSums(landData[, , c("primforest", "secdforest", "forestry_aff", "forestry_ndc", "forestry_plant")],
                 dim = 3)),
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
         dimSums(landData[, , c("forestry_aff", "forestry_ndc", "forestry_plant")], dim = 3))
  )

  s32AffPlantation <- readGDX(gdx, "s32_aff_plantation")
  if (s32AffPlantation == 0) {
    outputParts <- append(outputParts, list(
      list("Resources|Land Cover|Forest|Planted Forest|+|Plantations (million ha)",
           dimSums(landData[, , "forestry_plant"], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Plantations|+|Timber (million ha)",
           dimSums(landData[, , "forestry_plant"], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Plantations|+|CO2-price AR (million ha)",
           new.magpie(getItems(landData, 1.1), getYears(landData), NULL, fill = 0, sets = getSets(landData))),
      list("Resources|Land Cover|Forest|Planted Forest|+|Natural (million ha)",
           dimSums(landData[, , c("forestry_aff", "forestry_ndc")], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Natural|+|CO2-price AR (million ha)",
           dimSums(landData[, , "forestry_aff"], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Natural|+|NPI_NDC AR (million ha)",
           dimSums(landData[, , "forestry_ndc"], dim = 3))
    ))
  } else if (s32AffPlantation == 1) {
    outputParts <- append(outputParts, list(
      list("Resources|Land Cover|Forest|Planted Forest|+|Plantations (million ha)",
           dimSums(landData[, , c("forestry_plant", "forestry_aff")], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Plantations|+|Timber (million ha)",
           dimSums(landData[, , "forestry_plant"], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Plantations|+|CO2-price AR (million ha)",
           dimSums(landData[, , "forestry_aff"], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|+|Natural (million ha)",
           dimSums(landData[, , "forestry_ndc"], dim = 3)),
      list("Resources|Land Cover|Forest|Planted Forest|Natural|+|CO2-price AR (million ha)",
           new.magpie(getItems(landData, 1.1), getYears(landData), NULL, fill = 0, sets = getSets(landData))),
      list("Resources|Land Cover|Forest|Planted Forest|Natural|+|NPI_NDC AR (million ha)",
           dimSums(landData[, , "forestry_ndc"], dim = 3))
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
