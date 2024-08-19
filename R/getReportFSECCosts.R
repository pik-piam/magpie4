#' @title getReportFSECCosts
#' @description Reports cost indicators for the FSEC project
#'
#' @export
#'
#' @param gdx a GDX file
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#' @return A .csv containing the summed output of reportCostsAccounting on the region level
#' @author Michael Crawford
#' @importFrom dplyr %>% select mutate rename
#' @importFrom rlang .data
#' @importFrom utils write.csv
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECCosts(gdx)
#'   }
#'

getReportFSECCosts <- function(gdx, reportOutputDir = NULL, scenario = NULL) {

  costs <- reportCostsAccounting(gdx = gdx)

  # Costs relevant to the FSEC context
  costs <- costs[, , c("Costs Accounting|+|Land Conversion (million US$17/yr)",
                      "Costs Accounting|+|Transport (million US$17/yr)",
                      "Costs Accounting|+|N Fertilizer (million US$17/yr)",
                      "Costs Accounting|+|P Fertilizer (million US$17/yr)",
                      "Costs Accounting|+|MACCS (million US$17/yr)",
                      "Costs Accounting|+|AEI (million US$17/yr)",
                      "Costs Accounting|+|Trade (million US$17/yr)",
                      "Costs Accounting|+|Timber production (million US$17/yr)",
                      "Costs Accounting|+|Processing (million US$17/yr)",
                      "Costs Accounting|+|Reward for producing bioenergy (million US$17/yr)",
                      "Costs Accounting|+|Substitution processing (million US$17/yr)",
                      "Costs Accounting|+|Punishment cost for additionally transported monogastric livst_egg (million US$17/yr)",
                      "Costs Accounting|+|Land transition matrix (million US$17/yr)",
                      "Costs Accounting|+|Timber harvest natveg (million US$17/yr)",
                      "Costs Accounting|+|Input Factors (million US$17/yr)",
                      "Costs Accounting|+|Peatland (million US$17/yr)",
                      "Costs Accounting|+|Forestry (million US$17/yr)",
                      "Costs Accounting|+|TC (million US$17/yr)")]

  costs <- dimSums(costs, dim = 3)

  costs <- as.data.frame(costs) %>%
    dplyr::select(.data$Region, .data$Year, .data$Value) %>%
    dplyr::mutate(Value = round(.data$Value, digits = 2)) %>%
    dplyr::rename(`Costs (million US$17/yr)` = .data$Value)

  if (!is.null(reportOutputDir) & !is.null(scenario)) {
    write.csv(costs,
              file = file.path(reportOutputDir, paste0(scenario, "-CostsAccounting.csv")),
              row.names = FALSE, quote = TRUE)
  }

  return(costs)

}
