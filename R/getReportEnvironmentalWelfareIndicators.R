#' @title getReportEnvironmentalWelfareIndicators
#' @description Puts together a report of the environmental indicators for the Social Welfare Function
#'
#' @export
#'
#' @param gdx GDX file
#' @param folder a folder name the output should be written to using write.report. If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE). If NULL the report is returned instead as a MAgPIE object.
#' @param filter Modelstat filter. Here you have to set the modelstat values for which results should be used. All values for time steps in which the modelstat is different or for which one of the previous modelstats were different are set to NA.
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @param ... additional arguments for write.report. Will only be taken into account if argument "file" is not NULL.
#'
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @author Felicitas Beier, Michael Crawford
#'
#' @importFrom magclass write.report2 getSets<- getSets add_dimension is.magpie
#' @importFrom methods is
#' @importFrom iamc RenameAndAggregate
#'
#' @examples
#' \dontrun{
#' x <- getReportMAgPIE2LPJmL(gdx)
#' }
#'
getReportEnvironmentalWelfareIndicators <- function(gdx, folder = NULL, scenario = NULL, filter = c(2, 7), dir = ".", spamfiledirectory = "", ...) {

  ### Indicators to be reported:
  # Climate: ghg emissions; temperature(???) [prescribed temperature pathway] -> global (should be extracted from RCPs)
  # Biodiversity intactness: biodiversity intactness indicator; biodiversity hotspot areas (in terms of area) -> gridded
  # Nitrogen pollution: N surplus; AWM loss -> gridded

  #### Questions to Mike:
  # do we still use spamfiles?
  # which arguments are necessary?
  # how to run getReportGrid functions?

  ### NOTE: So far copy-paste of getReportMAgPIE2LPJmL adjusted for indicators of interest...

  dir <- getDirectory(dir, spamfiledirectory)

  tryReport <- function(reporting, gdx, filter, scenario) {
    file <- reporting[[2]]
    report <- reporting[[1]]
    regs  <- c(readGDX(gdx, "i"))
    years <- readGDX(gdx, "t")
    message("   ", format(report), appendLF = FALSE)
    x <- try(eval(parse(text = paste0(report))), silent = TRUE)
    if (is(x, "try-error")) {
      message("ERROR")
      x <- NULL
    } else if (is.null(x)) {
      message("no return value")
      x <- NULL
    } else if (!is.magpie(x)) {
      message("ERROR - no magpie object")
      x <- NULL
    } else if (!setequal(getYears(x), years)) {
      message("ERROR - wrong years")
      x <- NULL
    } else if (!setequal(getRegions(x), regs)) {
      message("ERROR - wrong regions")
      x <- NULL
    } else {
      message("success")

      x <- .filtermagpie(x, gdx, filter = filter)

      getSets(x, fulldim = FALSE)[3] <- "variable"

      if (!is.null(scenario)) x <- add_dimension(x, dim = 3.1, add = "scenario", nm = scenario)
      # x <- add_dimension(x, dim=3.1, add="model", nm="MAgPIE")


      if (!is.null(file)) write.magpie(x, file_name = file)
    }
  }

  message("Start getReport(gdx)...")

  reporting <- list(
    # list("reportGridLand(gdx,dir=dir)", paste0(folder,"LandAreaPhysical.nc")),
    # list("reportGridCroparea(gdx,dir=dir)", paste0(folder,"CroplandAreaPhysical.nc")),
    list("reportNitrogenBudgetCropland(gdx,grid=TRUE,dir=dir,include_emissions=TRUE)", paste0(folder, "NitrogenBudgetCropland.nc")), ##### BENNI: Is this N surplus????
    list("reportNitrogenBudgetPasture(gdx,grid=TRUE,include_emissions=TRUE,dir=dir)", paste0(folder, "NitrogenBudgetPasture.nc")),
    list("reportNitrogenBudgetNonagland(gdx,grid=TRUE,dir=dir)", paste0(folder, "NitrogenBudgetNonagland.nc")),
    list("reportGridManureExcretion(gdx,grid=TRUE,dir=dir)", paste0(folder, "NitrogenExcretion.nc"))     ##### BENNI: Is this what we would report for AWM loss?
    # list("reportGridYields(gdx,dir=dir)", paste0(folder,"CroplandAreaPhysical.nc")),
    # list("reportGridNitrogenWithdrawals(gdx,dir=dir)", paste0(folder,"CroplandAreaPhysical.nc")),
    # list("reportGridResidueDemandgdx,dir=dir)", paste0(folder,"CroplandAreaPhysical.nc")),

    ### BENNI: reportManure = reportGridAWMS?????

  )

  ### For biodiversity hotspots area reporting:
  # maybe: share of BH area that is natural land????

  ### or maybe: protectedAreas


  output <- lapply(X = reporting, FUN = tryReport, gdx = gdx, filter = filter, scenario = scenario)


  #### Available at country level:
  ### (Copied from getReportINMS and adjusted to include indicators of interest)

  tryList <- function(..., gdx) {
    width <- max(nchar(c(...))) + 1
    return(lapply(list(...), tryReport, width, gdx))
  }

  output <- tryList("reportBII(gdx)",
                    "reportProtectedArea(gdx)", ### ??????
                    gdx = gdx)

  output <- .filtermagpie(mbind(output), gdx, filter = filter)

  getSets(output, fulldim = FALSE)[3] <- "variable"

  if (!is.null(scenario)) output <- add_dimension(output, dim = 3.1, add = "scenario", nm = scenario)
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "MAgPIE")


  if (!is.null(file)) write.report2(output, file = file, ...)
  else return(output)

}
