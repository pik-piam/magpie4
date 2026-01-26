#' @title trade
#' @description Calculates MAgPIE trade or self-sufficiencies out of a gdx file
#'
#' @importFrom magclass where
#' @importFrom dplyr relocate
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo", or name of custom mapping)
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param productAggr aggregate over products or not (boolean)
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"),
#' reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param weight in case relative=T also the weighting for the self sufficiencies
#' is provided as it is an intensive parameter
#' @param relative if relative=TRUE, self sufficiencies are reported,
#' so the amount of production divided by domestic demand
#' @param type exports-imports ("net-exports"), gross imports ("imports") or
#' gross exports ("exports"), in the bilateral case we report a few others given
#' balanceflow: ; only valid if relative=FALSE
#' @details Trade definitions are equivalent to FAO CBS categories
#' @return trade (production-demand) as MAgPIE object; unit depends on attributes
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Mishko Stevanovic
#' @examples
#'
#'   \dontrun{
#'     x <- trade(gdx="fulldata.gdx", level="regglo", products="kcr")
#'   }
#'

trade <- function(gdx, file = NULL, level = "reg", products = "k_trade",
                  productAggr = FALSE, attributes = "dm", weight = FALSE,
                  relative = FALSE, type = "net-exports") {

  if (!all(products %in% readGDX(gdx, "kall"))) {
    products <- try(readGDX(gdx, products))
    if (is.null(products)) {
      products <- readGDX(gdx, "kall")
      warning("The specified commodity set in products argument does not exit.
              Instead the full kall set is given to products argument.")
    }
  }

  amtTraded <- suppressWarnings((readGDX(gdx, "ov21_trade")))

  checkForUnderOrOverproduction(gdx)

  production <- production(gdx, level = level, products = products,
                           product_aggr = FALSE, attributes = attributes)

  demand <- dimSums(demand(gdx, level = level, products = products,
                           product_aggr = FALSE, attributes = attributes),
                    dim = 3.1)

  # We get the original regions for the special aggregation to
  # glo or custom region aggregations below.
  originalRegions <- getItems(production(gdx, level = "reg", products = products,
                                         product_aggr = FALSE, attributes = attributes),
                              1)

  proddem <- mbind(
    addDim(production, dim = 3.1, dimName = "type", item = "production"),
    addDim(demand, dim = 3.1, dimName = "type", item = "demand")
  )

  if (relative) {
    if (productAggr) {
      proddem <- dimSums(proddem, dim = "kall")
    }
    if (weight) {
      x = dimSums(proddem[, , "production"], dim = 3.1) / # dimSums aggregates across attributes
        round(dimSums(proddem[, , "demand"], dim = 3.1), 8)
      weight = dimSums(proddem[, , "demand"], dim = 3.1) + 1e-8
      x[is.na(x) | is.infinite(x)] <- 0
      out <- list(x = x, weight = weight)
    } else {
      out <- dimSums(proddem[, , "production"], dim = 3.1) /
        round(dimSums(proddem[, , "demand"], dim = 3.1), 8)
      out[is.na(out) | is.infinite(out)] <- 0
    }
  } else {

    if (is.null(amtTraded)) {
      out <- dimSums(proddem[, , "production"], dim = 3.1) -
        dimSums(proddem[, , "demand"], dim = 3.1)

      if (type == "net-exports") {
        out <- out
      } else if (type == "exports") {
        out[out < 0] <- 0
        # recomputes aggregated regions (most often GLO) in case of "glo", "regglo", or custom mappings,
        # as aggregated prod-dem which will always be ~0 with sum of imports. Same for imports below.
        out <- gdxAggregate(gdx, out[originalRegions, , ], to = level)
      } else if (type == "imports") {
        out[out > 0] <- 0
        out <- -1 * out
        out <- gdxAggregate(gdx, out[originalRegions, , ], to = level)
      } else {
        stop("unknown type")
      }

      if (productAggr) {
        out <- dimSums(out, dim = "kall")
      }

    } else {


      # first read in the balanceflows
      exportBf <- readGDX(gdx, "f21_trade_export_balanceflow", react = "silent")
      regBf <- readGDX(gdx, "f21_trade_regional_balanceflow", react = "silent")

      import <- dimSums(amtTraded, dim = "i_ex")[, , "level", drop = TRUE]

      # switch dims around
      import <- as.data.frame(import, rev = 2)
      import <- dplyr::relocate(import, "i_im", .before = 1)
      import <- as.magpie(import, spatial = 1, temporal = 2, tidy = TRUE)

      export <- dimSums(amtTraded, dim = "i_im")[, , "level", drop = TRUE]

      if (type == "net-exports") {
        out <- export - import
        if (level %in% c("glo", "regglo")) {
          outG <- round(production(gdx, level = "glo") - dimSums(demand(gdx, level = "glo"),
                                                                 dim = 3.1),
                        digits = 7)[, , getItems(out, dim = 3)]
          getItems(outG, dim = 1) <- "GLO"
          out <- mbind(out, outG)
        } else if (!(level %in% c("glo", "regglo", "reg"))) {
          stop("trade on a run that includes ov21_trade currently only supports reg, regglo, glo. Got: ", level)
        }
      } else if (type == "imports") {
        out <- gdxAggregate(gdx, import["GLO", , invert = TRUE], to = level)
      } else if (type == "exports") {
        export <- export + exportBf[, getYears(export), ]
        out <- gdxAggregate(gdx, export["GLO", , invert = TRUE], to = level)
      } else if (type == "exportsExclBf") {
        out <- gdxAggregate(gdx, export["GLO", , invert = TRUE], to = level)
      }

    }

    if (weight) {
      out <- list(x = out, weight = NULL)
    } else {
      out <- out
    }
  }

  if (is.list(out)) {
    return(out)
  } else {
    out(out, file)
  }
}

checkForUnderOrOverproduction <- function(gdx) {
  ## The messages below seem to get triggered by extremely low values in diff.
  ## Could be a rounding issue. Rounding to 7 digits should be safe because we deal in 10e6 values mostly.
  diff <- round(production(gdx, level = "glo") -
                  dimSums(demand(gdx, level = "glo"), dim = 3.1),
                digits = 7)
  balanceflow <- readGDX(gdx, "f21_trade_balanceflow", react = "silent")

  if (is.null(balanceflow)) {
    balanceflow <- readGDX(gdx, "fm_trade_balanceflow", react = "silent")
    ## Needs to be converted to interface for timber module WIP
  }

  # Only take the years and products that are in diff and balanceflow
  balanceflow <- balanceflow[, getYears(diff), ]
  diff <- diff[, , getNames(balanceflow)] - balanceflow

  # Check for over- and underproduction
  if (any(round(diff, 2) > 0)) {
    message("\nFor the following categories, overproduction is noticed (on top of balanceflow): \n",
            paste(unique(as.vector(where(round(diff, 2) > 0)$true$individual[, 3])), collapse = ", "), "\n")
  }
  if (any(round(diff, 2) < 0)) {
    warning("For the following categories, underproduction (on top of balanceflow): \n",
            paste(unique(as.vector(where(round(diff, 2) < 0)$true$individual[, 3])), collapse = ", "), "\n")
  }
}
