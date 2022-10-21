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

#million people
population <- population(gdx, level = level)

#Cropland - million hectares
land <- land(gdx, level = level, types = NULL, subcategories = c("forestry"), sum = FALSE)

#Quantity of trade of crops in million tonnes
trade <- trade(gdx, level = level, products = "kcr", type = "net-exports")

#tonnes/hectare
yield <- yields(gdx, level = "regglo", products = "kcr", product_aggr = FALSE,
                  attributes = "dm", water_aggr = TRUE)

#land needed to produce crops for trade (million hectares)
landTrade <- trade / yield
landTradetotal <- dimSums(landTrade, dim = 3, na.rm = TRUE)


#million hectare
totalLand <- land[, , "crop.total"] + land[, , "past.total"] + landTradetotal

#million hectare / million people
landFootprint <- totalLand / population

getNames(landFootprint) <- "Productivity|Land for Food Production (hectares per capita))"

return(landFootprint)

}
