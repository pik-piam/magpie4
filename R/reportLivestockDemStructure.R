#' @title reportLivestockDemStructure
#' @description reports the share of different livestock products (excluding fish) in total livestock calorie food supply
#' 
#' @export
#'
#' @param gdx GDX file
#' @return livestock demand structure as MAgPIE object (kcal/kcal)
#' @author Isabelle Weindl
#' @importFrom magpiesets findset
#' @examples
#' 
#'   \dontrun{
#'     x <- reportLivestockDemStructure(gdx)
#'   }
#' 
#'
#' @section Livestock demand structure variables:
#' Name | Unit | Meta
#' ---|---|---
#' Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Ruminant meat | kcal/kcal | Share of ruminant meat in livestock calorie supply
#' Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Poultry meat and eggs | kcal/kcal | Share of poultry and eggs in livestock calorie supply
#' Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Dairy | kcal/kcal | Share of dairy in livestock calorie supply
#' Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Monogastric meat | kcal/kcal | Share of monogastric meat in livestock calorie supply
#' @md


reportLivestockDemStructure <- function(gdx, level = "regglo") {
  out <- LivestockDemStructure(gdx, level = level, attributes = "kcal", fish = FALSE)

  group <- "kli"
  products <- findset("kli")
  level_zero_name <- "Nutrition|Dietary Composition|Livestock Demand Structure"
  out <- reporthelper(x = out, level_zero_name = level_zero_name, detail = TRUE)

  nosum  <- out[, , paste(level_zero_name, reportingnames(group), sep = "|"), invert = TRUE]
  out <- summationhelper(nosum)
  getNames(out) <- paste(getNames(out), "(kcal/kcal)", sep = " ")

  return(out)
}
