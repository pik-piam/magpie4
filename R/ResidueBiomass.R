#' @title ResidueBiomass
#' @description reads Crop Residue Biomass out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not. Usually boolean, but here also the value "kres" is allowed, which provides kcr aggregated to kres
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param plantpart both ag or bg
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{reportProduction}}, \code{\link{demand}}
#' @examples
#' \dontrun{
#' x <- production(gdx)
#' }
#'
ResidueBiomass <- function(gdx, level = "reg", dir = ".", spamfiledirectory = "",
                           products = "kcr", product_aggr = FALSE, attributes = "dm",
                           water_aggr = TRUE, plantpart = "both") {

  dir <- getDirectory(dir, spamfiledirectory)

  if (!all(products %in% findset("kcr"))) {
    products <- readGDX(gdx, products)
  }
  if (product_aggr == "kres") {
    products <- setdiff(products, c("sunflower", "oilpalm", "foddr", "begr", "betr"))
  }

  area       <- croparea(gdx = gdx, level = level, products = products,
                         product_aggr = FALSE, water_aggr = water_aggr, dir = dir)
  production <- production(gdx = gdx, level = level, products = products,
                           product_aggr = FALSE, water_aggr = water_aggr, dir = dir)
  multi <- readGDX(gdx, "f18_multicropping", "fm_multicropping",
                   format = "first_found")[, getYears(area), ]
  cgf   <- readGDX(gdx, "f18_cgf")[, , getNames(area, dim = 1)]
  attributes_ag <- readGDX(gdx, "f18_attributes_residue_ag")[, , getNames(area, dim = 1)][, , attributes]
  attributes_bg <- readGDX(gdx, "f18_attributes_residue_bg")[, , getNames(area, dim = 1)][, , attributes]

  # aggregate parameters to right resolution
  # weight <- land(gdx,types = "crop",level=level,dir=dir)
  multi <- gdxAggregate(gdx = gdx, x = multi, weight = NULL, to = level,
                        absolute = FALSE, dir = dir)

  ag <- area * multi * collapseNames(cgf[, , "intercept"]) +
         production * collapseNames(cgf[, , "slope"])
  bg <- (ag + production) * collapseNames(cgf[, , "bg_to_ag"])

  res <- mbind(
    add_dimension(ag * attributes_ag, dim = 3.1, nm = "ag"),
    add_dimension(bg * attributes_bg, dim = 3.1, nm = "bg")
  )

  if (product_aggr == TRUE) {
    res <- dimSums(res, dim = 3.2)
  } else if (product_aggr == "kres") {
    res <- collapseNames(res[, , "ag"], collapsedim = 1)
    map <- readGDX(gdx, "kres_kcr")
    cat("Note that not all kcrs are included in kres")

    warning("to be replaced once the gams code is updated")

    map <- map[which(map[, 2] %in% products), ]
    res <- speed_aggregate(res,
                           rel = map, from = "kcr", to = "kres", dim = 3.1)
  } else if (product_aggr != FALSE) {
    stop("unknown product_aggr")
  }

  if (plantpart != "both") {
    res <- res[, , plantpart]
  }

  return(res)
}
