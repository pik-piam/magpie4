#' @title reportProtein
#' @description reports per-capita protein food supply (including household waste)

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return per-capita protein as MAgPIE object (protein/cap/day)
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Abhijeet Mishra, Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportKcal(gdx)
#'   }
#'
#'
#' @section Protein supply variables:
#' Name | Unit | Meta
#' ---|---|---
#' Nutrition\|Protein Supply | protein/capita/day | Total per-capita protein supply (including household waste)
#' Nutrition\|Protein Supply\|+\|Crops | protein/capita/day | Protein supply from crops
#' Nutrition\|Protein Supply\|+\|Livestock products | protein/capita/day | Protein supply from livestock products
#' Nutrition\|Protein Supply\|+\|Secondary products | protein/capita/day | Protein supply from secondary/processed products
#' @md


reportProtein <- function(gdx, detail = FALSE, level = "regglo") {

  level_zero_name <- "Nutrition|Protein Supply"

  out <- collapseNames(
    Kcal(gdx, level = level, products = "kall", product_aggr = FALSE,
         calibrated = TRUE, magpie_input = FALSE, attributes = "protein"),
    collapsedim = 2
  )

  out <- reporthelper(x = out,
                      level_zero_name = level_zero_name,
                      detail = detail)

  if (level_zero_name %in% getNames(out)) {
    sumup  <- getNames(out[, , level_zero_name, invert = TRUE])
    getNames(out)  <- c(level_zero_name, getNames(summationhelper(out[, , sumup], sep = "+", dim = 3.1)))
  } else {
    getNames(out) <- getNames(summationhelper(out, sep = "+", dim = 3.1))
  }

  #delete empty categories
  keep <- "Nutrition|Protein Supply|Secondary products|+|Microbial protein"
  out <- out[, , unique(getNames(out)[which(dimSums(out, dim = c(1, 2)) != 0)], keep)]

  getNames(out) <- paste(getNames(out), "(protein/capita/day)", sep = " ")

  out <- out[, , sort(getNames(out))]

  return(out)
}
