#' @title getReportFSECAlessandroPassaro
#' @description Collects reports for Alessandro Passaro's analysis
#'
#' @export
#'
#' @param magpieOutputDir a magpie output directory which contains all the files associate with the given scenario
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#'
#' @return A list of reports
#' @author Michael Crawford
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#' @importFrom madrat toolConditionalReplace
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECAlessandroPassaro(magpieOutputDir)
#'   }
#'

getReportFSECAlessandroPassaro <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

    # --------------------------------------------------------------------------------
    # Helper functions

    .saveCSVReport <- function(x, file) {
        if (!is.null(reportOutputDir) && !is.null(scenario)) {
            write.csv(x,
                      file = file.path(reportOutputDir, paste0(scenario, "-", file, ".csv")),
                      row.names = FALSE)
        }
    }


    # --------------------------------------------------------------------------------
    # Poverty

    message("getReportFSECAlessandroPassaro: Collecting poverty datasets")

    reportISO_path <- file.path(magpieOutputDir, "report_iso.rds")
    povertyReport  <- readRDS(reportISO_path)

    povertyVariables <- c("Income|Income after Climate Policy",
                          "Income|Gini Coefficient",
                          "Income|Fraction of Population below half of Median Income",
                          "Income|Average Income of Lower 40% of Population",
                          "Income|Number of People Below 1p90 USDppp11/day",
                          "Income|Number of People Below 3p20 USDppp11/day",
                          "Income|Number of People Below 5p50 USDppp11/day")

    povertyReport <- povertyReport %>% filter(.data$variable %in% povertyVariables)

    if (nrow(povertyReport) > 0) {
        colnames(povertyReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
        .saveCSVReport(povertyReport, file = "poverty")
    } else {
        message("The poverty variables weren't found in the report_iso.rds for scenario: ", scenario)
    }

    # --------------------------------------------------------------------------------
    # Total and cumulative CO2e

    message("getReportFSECAlessandroPassaro: Collecting total and cumulative CO2eq emissions")

    report_path <- file.path(magpieOutputDir, "report.rds")
    co2eReport  <- readRDS(report_path)

    co2eVariables <- c("Emissions|GWP100AR6|Land",
                       "Emissions|GWP100AR6|Land|Cumulative")

    co2eReport <- co2eReport %>% filter(.data$variable %in% co2eVariables)

    if (nrow(co2eReport) > 0) {
        colnames(povertyReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
        .saveCSVReport(co2eReport, file = "CO2e")
    } else {
        message("The CO2e variables weren't found in the report.rds for scenario: ", scenario)
    }

    # --------------------------------------------------------------------------------
    # Return

    return(list(poverty    = povertyReport,
                co2eReport = co2eReport))
}
