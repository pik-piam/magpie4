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
#' @importFrom magpiesets findset

reportTrade <- function(gdx, detail = FALSE) {

  x <- NULL

  # net-exports
  out <- trade(gdx, level = "regglo", type = "net-exports", products = "kall")
  #remove trade of e-14 and so
  out <- round(out, 8)

  out <- reporthelper(x = out, dim = 3.1, level_zero_name = "Trade|Net-Trade",
                      detail = detail, partly = TRUE)
  getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
  x <- mbind(x, out)
  x <- summationhelper(x, excludeLevels = 1)

  # gross exports
  out <- round(trade(gdx, level = "regglo",  products = "kall", type = "exports"), 8)

  out <- reporthelper(x = out, dim = 3.1,
                      level_zero_name = "Trade|Exports", detail = detail, partly = TRUE)
  getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
  out <- summationhelper(out, excludeLevels = 1)
  x   <- mbind(x, out)

  # gross imports
  out <- round(trade(gdx, level = "regglo", type = "imports", products = "kall"), 8)

  out <- reporthelper(x = out, dim = 3.1, level_zero_name = "Trade|Imports", detail = detail, partly = TRUE)
  getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
  out <- summationhelper(out, excludeLevels = 1)
  x <- mbind(x, out)

  # self_sufficiency
  selfSufficiency <- suppressMessages(trade(gdx, level = "regglo", products = "kall", relative = TRUE, weight = TRUE))
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