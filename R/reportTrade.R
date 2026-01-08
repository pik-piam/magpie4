#' @title reportTrade
#' @description reports trade
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if true, provides estimates for all commodities, otherwise aggregates some groups
#' @return Net-Exports and self sufficiency (exports/domestic supply) as MAgPIE object. Unit: see names
#' @author Benjamin Leon Bodirsky, Mishko Stevanovic
#' @examples
#'
#'   \dontrun{
#'     x <- reportTrade(gdx="fulldata.gdx",detail=TRUE)
#'   }
#'
#' @section Net trade variables:
#' Name | Unit | Meta
#' ---|---|---
#' Trade\|Net-Trade\|+\|Crops | Mt DM/yr | Net export of crops (positive = net exporter)
#' Trade\|Net-Trade\|+\|Livestock products | Mt DM/yr | Net export of livestock products (excluding fish)
#' Trade\|Net-Trade\|+\|Secondary products | Mt DM/yr | Net export of secondary products
#' Trade\|Net-Trade\|+\|Bioenergy crops | Mt DM/yr | Net export of bioenergy crops
#'
#' @section Gross trade variables:
#' Name | Unit | Meta
#' ---|---|---
#' Trade\|Exports\|+\|Crops | Mt DM/yr | Gross exports of crops
#' Trade\|Exports\|+\|Livestock products | Mt DM/yr | Gross exports of livestock products
#' Trade\|Imports\|+\|Crops | Mt DM/yr | Gross imports of crops
#' Trade\|Imports\|+\|Livestock products | Mt DM/yr | Gross imports of livestock products
#'
#' @section Self-sufficiency variables:
#' Name | Unit | Meta
#' ---|---|---
#' Trade\|Self-sufficiency\|+\|Crops | 1 | Self-sufficiency ratio for crops (production/domestic supply)
#' Trade\|Self-sufficiency\|+\|Livestock products | 1 | Self-sufficiency ratio for livestock products
#' @md

#' @importFrom magpiesets findset

reportTrade <- function(gdx, detail = FALSE, level = "regglo") {

  x <- NULL

  .convertToReportFormat <- function(magpieObject, levelName) {
    out <- reporthelper(x = magpieObject, dim = 3.1, level_zero_name = levelName,
                        detail = detail, partly = TRUE)
    getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
    out <- summationhelper(out, excludeLevels = 1)
    return(out)
  }

  # net-exports
  out <- round(trade(gdx, level = level, type = "net-exports", products = "kall"), 8) # remove trade of e-14 and so
  x <- mbind(x, .convertToReportFormat(out, "Trade|Net-Trade"))

  # gross exports
  out <- round(trade(gdx, level = level,  products = "kall", type = "exports"), 8)
  x <- mbind(x, .convertToReportFormat(out, "Trade|Exports"))

  # gross imports
  out <- round(trade(gdx, level = level, type = "imports", products = "kall"), 8)
  x <- mbind(x, .convertToReportFormat(out, "Trade|Imports"))

  # self_sufficiency
  selfSufficiency <- suppressMessages(trade(gdx, level = level, products = "kall", relative = TRUE, weight = TRUE))
  weight    <- selfSufficiency$weight
  selfSufficiency <- selfSufficiency$x
  out <- reporthelper(x = selfSufficiency * weight, dim = 3.1,
                      level_zero_name = "Trade|Self-sufficiency",
                      detail = detail) / reporthelper(x = weight,
                                                      dim = 3.1,
                                                      level_zero_name = "Trade|Self-sufficiency",
                                                      detail = detail)

  getNames(out) <- paste(getNames(out), "(1)", sep = " ")
  x <- mbind(x, out)

  return(x)
}
