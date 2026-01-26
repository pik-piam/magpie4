#' @title NitrogenBudgetWithdrawals
#' @description calculates projections of Nitrogen Budgets withdrawals for Croplands from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param kcr "sum" provides the totals over all crops, "kcr" provides outputs by kcr
#' @param net TRUE only provides total net-withdrawals, otherwise all categories are
#' returned (fixation and seed are returned positive, not negative)
#' @param level aggregation level, reg, glo or regglo, cell, grid or iso
#' @author Benjamin Leon Bodirsky, Michael Crawford
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @examples
#' \dontrun{
#' x <- NitrogenBudgetWithdrawals(gdx)
#' }
#'
NitrogenBudgetWithdrawals <- function(gdx, kcr = "sum", net = TRUE, level = "reg") {


  if (level %in% c("cell", "reg", "grid", "iso")) {

    harvest_detail <- production(gdx, products = "kcr", attributes = "nr", level = level)
    if (kcr == "sum") {
      harvest <- dimSums(harvest_detail, dim = 3)
    } else if (kcr == "kcr") {
      harvest <- harvest_detail
    } else if (kcr != "kcr") {
      stop("unknown setting for kcr")
    }


    res_detail <- collapseNames(ResidueBiomass(gdx, product_aggr = FALSE, attributes = "nr", level = level))
    if (kcr == "sum") {
      res <- dimSums(res_detail, dim = 3.2)
      ag <- res[, , "ag"]
      bg <- res[, , "bg"]
    } else if (kcr == "kcr") {
      res <- res_detail
      ag <- res_detail[, , "ag"]
      bg <- res_detail[, , "bg"]
    } else if (kcr != "kcr") {
      stop("unknown setting for kcr")
    }

    seed_detail <- Seed(gdx, level = level, attributes = "nr")
    if (kcr == "sum") {
      seed <- dimSums(seed_detail, dim = 3)
    } else if (kcr == "kcr") {
      seed <- seed_detail
    } else if (kcr != "kcr") {
      stop("unknown setting for kcr")
    }


    fixation_crops <- harvest_detail + dimSums(res_detail, dim = 3.1)
    fixation_rate <- readGDX(gdx, "f50_nr_fix_ndfa")[, getYears(harvest)]
    fixation_rate <- gdxAggregate(gdx, fixation_rate, weight = NULL, to = level, absolute = FALSE)

    if (kcr == "sum") {
      fixation_crops <- dimSums(fixation_rate * fixation_crops, dim = 3)
    } else if (kcr == "kcr") {
      fixation_crops <- fixation_rate * fixation_crops
    } else if (kcr != "kcr") {
      stop("unknown setting for kcr")
    }

    if (net) {
      harvest <- setNames(harvest, paste0(getNames(harvest, dim = 1), ".harvest"))
      ag <- setNames(ag, paste0(getNames(ag, dim = 2), ".ag"))
      bg <- setNames(bg, paste0(getNames(bg, dim = 2), ".bg"))
      fixation_crops <- -setNames(fixation_crops, paste0(getNames(fixation_crops, dim = 1), ".fixation_crops"))
      seed <- -setNames(seed, paste0(getNames(seed, dim = 1), ".seed"))

      out <- harvest + ag + bg + fixation_crops + seed
      out <- if (kcr == "sum") dimSums(out, dim = 3) else if (kcr == "kcr") setNames(out, getNames(out, dim = 1))
      out[out < 0 & out > -1e-10] <- 0

    } else {
      if (kcr == "sum") {
        out <- mbind(
          setNames(harvest, "harvest"),
          setNames(ag, "ag"),
          setNames(bg, "bg"),
          setNames(fixation_crops, "fixation_crops"),
          setNames(seed, "seed")
        )
      } else if (kcr == "kcr") {

        out <- mbind(
          setNames(harvest, paste0(getNames(harvest, dim = 1), ".harvest")),
          setNames(ag, paste0(getNames(ag, dim = 2), ".ag")),
          setNames(bg, paste0(getNames(bg, dim = 2), ".bg")),
          setNames(fixation_crops, paste0(getNames(fixation_crops, dim = 1), ".fixation_crops")),
          setNames(seed, paste0(getNames(seed, dim = 1), ".seed"))
        )
      }
    }


    ### error checks

    if (any(out < 0)) {
      stop("no values should be negative")
    }
    return(out)

  } else { # All other levels
    return(superAggregateX(NitrogenBudgetWithdrawals(gdx, kcr = kcr, net = net, level = "reg"),
                           aggr_type = "sum",
                           level = level))
  }

}
