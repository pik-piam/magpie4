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

  nontraded <- readGDX(gdx, "k_notrade")

  x <- NULL

  # net-exports
  out <- trade(gdx,level = "regglo",type = "net-exports")
  # add non-traded goods with value of 0
  out <- add_columns(out, addnm = nontraded, dim = 3)
  out[, , nontraded] <- 0

  out <- reporthelper(x = out, dim = 3.1, level_zero_name = "Trade|Net-Trade",
                      detail = detail, partly = TRUE)
  getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
  x <- mbind(x,out)
  x <- summationhelper(x, excludeLevels = 1)

  # # gross exports
   out <- trade(gdx,level = "regglo",type = "exports")
   # add non-traded goods with value of 0
   out <- add_columns(out, addnm = nontraded, dim = 3)
   out[, , nontraded] <- 0

   out <- reporthelper(x = out, dim = 3.1,
                       level_zero_name = "Trade|Exports", detail = detail, partly = TRUE)
   getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
   out <- summationhelper(out, excludeLevels = 1)
   x   <- mbind(x,out)
  #
  # # gross imports
   out <- trade(gdx,level = "regglo",type = "imports")
   # add non-traded goods with value of 0
   out <- add_columns(out, addnm = nontraded, dim = 3)
   out[, , nontraded] <- 0

   out <- reporthelper(x = out, dim = 3.1,level_zero_name = "Trade|Imports", detail = detail, partly = TRUE)
   getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep=" ")
   out <- summationhelper(out, excludeLevels = 1)
   x <- mbind(x, out)

  # self_sufficiency
  self_suff <- suppressMessages(trade(gdx, level = "regglo", relative = TRUE, weight = TRUE))
  weight    <- self_suff$weight
  self_suff <- self_suff$x
  out <- (reporthelper(x = self_suff * weight, dim = 3.1,
                       level_zero_name = "Trade|Self-sufficiency",
                       detail = detail) / reporthelper(x = weight,
                                                       dim = 3.1,
                                                       level_zero_name = "Trade|Self-sufficiency",
                                                       detail = detail))

  getNames(out) <- paste(getNames(out), "(1)", sep = " ")
  x <- mbind(x, out)

  return(x)
}
