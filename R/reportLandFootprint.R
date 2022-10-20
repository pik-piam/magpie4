#' @title reportLandFootprint
#' @description reports land footprint of food production for aggregated per-capita kcal intake including exogenous scenarios

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return land use for food production per capita (million hectares per-capita)
#' @author Vartika Singh
#' @examples
#'
#'   \dontrun{
#'     x <- reportLandFootprint(gdx)
#'   }
#'

reportLandFootprint <- function(gdx){

population <- population(gdx, level = "regglo")

#reading land information
land <- land(gdx, level = "regglo", types = NULL, subcategories = c("forestry"), sum = FALSE)

#total land for food production
totalLand <- land[, ,"crop.total"] + land[, ,"past.total"]

landFootprint <- totalLand / population

getNames(landFootprint) <- "Land footprint of food production (hectares per capita)"

return(landFootprint)

}
