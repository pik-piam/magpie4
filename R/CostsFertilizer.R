#' @title CostsFertilizer
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param disagg whether costs should be disaggregated into the different crop types
#' @return MAgPIE object containing fertilizer costs [million US$17]
#' @author Debbora Leip
#' @importFrom magclass dimSums
#' @examples
#'   \dontrun{
#'     x <- CostsFertilizer(gdx)
#'   }
#' @export
CostsFertilizer <- function(gdx, file = NULL, level = "regglo", disagg = TRUE) {

  nr_fertilizer_costs <- readGDX(gdx,"ov_nr_inorg_fert_costs", react = "silent", format = "first_found", select = list(type = "level"))

  # disaggregate into crop types
  if (disagg) {
    nr_withdrawals <- readGDX(gdx, "ov50_nr_withdrawals", format = "first_found", select = list(type = "level"))
    nr_withdrawals_shares <- nr_withdrawals/dimSums(nr_withdrawals, dim = "kcr")
    nr_fertilizer_costs <- nr_fertilizer_costs * nr_withdrawals_shares
  }

  nr_fertilizer_costs <- superAggregateX(nr_fertilizer_costs, aggr_type = "sum", level = level)

  out(nr_fertilizer_costs, file)
}
