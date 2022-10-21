#' @title reportLandFootprint
#' @description reports land footprint of food production for the population in a region
#' accounting for traded food assuming perfect substitution between domestic production
#' and imports

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return land use for food production per capita (million hectares per-capita)
#' @author Vartika Singh
#' @examples
#'
#'   \dontrun{
#'     x <- reportLandFootprint(gdx)
#'   }
#'

reportLandFootprint <- function(gdx, level = "regglo") {

population <- population(gdx, level = level)

#reading land information
land <- land(gdx, level = level, types = NULL, subcategories = c("forestry"), sum = FALSE)

#Value of trade of crops in million tonnes
out <- trade(gdx, level = level, products = "kcr", type = "net-exports")

#land needed to produce the traded commodities will be traded quantity (m tonnes)
# divided by yield (m tonnes/ha)
yield <- yields(gdx, level = "regglo", products = "kcr", product_aggr = FALSE,
                  attributes = "dm", water_aggr = TRUE)

landTrade <- out / yield
landTradetotal <- dimSums(landTrade, dim = 3, na.rm = TRUE)


#total land for food production
totalLand <- land[, , "crop.total"] + land[, , "past.total"] + landTradetotal

landFootprint <- totalLand / population

getNames(landFootprint) <- "Land footprint of food production (hectares per capita)"

return(landFootprint)

}
