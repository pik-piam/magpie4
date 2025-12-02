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
           new.magpie(getRegions(landData), getYears(landData), NULL, fill = 0, sets = getSets(landData))),
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
           new.magpie(getRegions(landData), getYears(landData), NULL, fill = 0, sets = getSets(landData))),
      list("Resources|Land Cover|Forest|Planted Forest|Natural|+|NPI_NDC AR (million ha)",
           dimSums(landData[, , "forestry_ndc"], dim = 3)),
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
