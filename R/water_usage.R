#' @title water_usage
#' @description reads area usage from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx    GDX file
#' @param file   a file name the output should be written to using write.magpie
#' @param level  spatial level of aggregation: "grid" (grid-cell) "cell" (cellular),
#'               "reg" (regional), "glo" (global), "regglo" (regional and global) or
#'               any other aggregation level defined in gdxAggregate
#' @param users  NULL or "sectors" or "kcr" or "kli".
#'               If NULL, all sectors including crop-wise water use and livestock will be obtained.
#'               If sectors, will only report for high-level sectors - agriculture, industry, electricity, domestic, ecosystem.
#'               Sum is applicable only in the case of sectors
#' @param sum    determines whether output should be sector specific (FALSE) or aggregated over all sectors (TRUE)
#' @param seasonality water usage time of the year. options: "grper" (growing period) or "total" (entire year).
#'                    Note: currently only implemented for non-agricultural water usage.
#' @param abstractiontype water usage abstraction type: "withdrawal" or "consumption"
#' @param digits integer. For rounding of the return values
#' @param dir    for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#'
#' @importFrom magclass collapseNames
#'
#' @return A MAgPIE object containing the water usage (km^3/yr)
#' @author Markus Bonsch, Vartika Singh, Felicitas Beier
#' @examples
#' \dontrun{
#' x <- water_usage(gdx)
#' }
#'

water_usage <- function(gdx, file = NULL, level = "reg", users = NULL,
                        sum = FALSE, seasonality = "total", abstractiontype = "withdrawal",
                        digits = 4, dir = ".") {

  sectors <- readGDX(gdx, "wat_dem")
  kcr     <- readGDX(gdx, "kcr")
  kli     <- readGDX(gdx, "kli")

  i42_watdem_total <- readGDX(gdx, "i42_watdem_total", types = "parameters")
  # For backwards compatibility (for MAgPIE versions <4.8.0)
  if (!any(grepl("withdrawal", getItems(i42_watdem_total, dim = 3)))) {
    tmp <- i42_watdem_total
    i42_watdem_total <- new.magpie(cells_and_regions = getItems(i42_watdem_total, dim = 1),
                                   years = getItems(i42_watdem_total, dim = 2),
                                   names = c(paste(getItems(i42_watdem_total, dim = 3), "consumption", sep = "."),
                                             paste(getItems(i42_watdem_total, dim = 3), "withdrawal", sep = ".")),
                                   fill = NA)
    i42_watdem_total[, , "withdrawal"] <- tmp
  }

  if (is.null(users)) {
    users <- c(sectors, kcr, kli)
  } else {
    users <- NULL
    if ("sectors" %in% users) users <- c(users, sectors)
    if ("kcr" %in% users)     users <- c(users, "kcr")
    if ("kli" %in% users)     users <- c(users, "kli")
  }

  user       <- list()

  user$crops <- match(users, kcr)
  user$crops <- user$crops[!is.na(user$crops)]
  if (length(user$crops) > 0) {
    user$crops <- kcr[user$crops]
  }

  user$kli <- match(users, kli)
  user$kli <- user$kli[!is.na(user$kli)]
  if (length(user$kli) > 0) {
    user$kli <- kli[user$kli]
  }

  user$sectors <- match(users, sectors)
  user$sectors <- user$sectors[!is.na(user$sectors)]
  if (length(user$sectors) > 0) {
    user$sectors <- sectors[user$sectors]
  }

  # Test if all users belong either to kcr or sectors
  if (length(unlist(user)) != length(users)) {
    bad <- users[which(is.na(match(users, unlist(user))))]
    stop(paste("ERROR: The following elements of users are not water users: ", bad))
  }
  if ((length(user$crops) > 0 | length(user$kli) > 0) && length(user$sectors) > 0 && sum == TRUE) {
    warning("Summing over specific crops and other sectors in water_usage")
  }

  out <- list()
  if (length(user$sectors) > 0) {
    out$sectors <- readGDX(gdx, "ov_watdem", "ovm_watdem",
                           format = "first_found")[, , "level"][, , user$sectors]
    out$sectors <- setNames(out$sectors,
                            gsub(".level", "", getNames(out$sectors), fixed = TRUE))

    # Non-agricultural water usage is reported for the entire year
    # (not only the growing period as for agricultural water usage)
    if (any(grepl(pattern = 'domestic|manufacturing|electricity', x = user$sectors))) {
      # extract total water abstraction
      i42_watdem_total_ww <- collapseNames(i42_watdem_total[, , "withdrawal"])
      selectedSector   <- intersect(user$sectors, getItems(i42_watdem_total_ww, dim = 3))
      if (seasonality == "total") {
        # assign total water demand for non-agricultural sectors
        out$sectors[, , selectedSector] <- i42_watdem_total_ww[, getItems(out$sectors, dim = 2), selectedSector]
        # helper parameter to scale consumption
        ratioGrperTotal <- ifelse(i42_watdem_total_ww[, getItems(out$sectors, dim = 2), selectedSector] > 0,
                                   out$sectors[, , selectedSector] / i42_watdem_total_ww[, getItems(out$sectors, dim = 2), selectedSector],
                                  1)
      } else if (seasonality == "grper") {
        ratioGrperTotal <- ifelse(i42_watdem_total_ww[, getItems(out$sectors, dim = 2), selectedSector] > 0,
                                   out$sectors[, , selectedSector] / i42_watdem_total_ww[, getItems(out$sectors, dim = 2), selectedSector],
                                  1)
      } else {
        stop("Please choose seasonality argument in magpie4::water_usage() function.")
      }
      if (any(ratioGrperTotal > 1)) {
        stop("More water in growing period than in entire year.
             Please double-check starting from magpie4::water_usage()")
      }
    }
  }

  if (length(user$crops) > 0) {
    i_wat_req_k_cell <- readGDX(gdx, "i42_wat_req_k", "i43_wat_req_k", "pm_wat_req_k",
                                format = "first_found")[, , user$crops]

    if (is.null(i_wat_req_k_cell)) {
      warning("Water usage cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
      return(NULL)
    }

    ovm_area_cell <- croparea(gdx, level = "cell", products = user$crops,
                              product_aggr = FALSE, water_aggr = FALSE)[, , "irrigated"]
    ovm_area_cell <- setNames(ovm_area_cell,
                              gsub(".irrigated", "", getNames(ovm_area_cell), fixed = TRUE))

    # For backwards compatibility only
    tmp <- readGDX(gdx, "ov42_irrig_eff", "ov43_irrig_eff", "ov17_irrig_eff", "i42_irrig_eff",
                   format = "first_found")

    if (length(getNames(tmp)) > length(as.matrix(readGDX(gdx, "type", format = "first_found")))) {
      ov_irrig_eff_cell <- tmp[, , "level"][, , user$crops]
    } else {
      ov_irrig_eff_cell_tmp <- tmp[, , "level"]
      ov_irrig_eff_cell <- ovm_area_cell
      for (crop in user$crops) {
        ov_irrig_eff_cell[, , crop] <- setNames(ov_irrig_eff_cell_tmp, NULL)
      }
    }

    # harmonize temporal dimension
    i_wat_req_k_cell <- i_wat_req_k_cell[, getItems(ovm_area_cell, dim = 2), ]

    out$kcr <- ovm_area_cell * i_wat_req_k_cell / ov_irrig_eff_cell
  }

  if (length(user$kli) > 0) {

    i_wat_req_k_cell <- readGDX(gdx, "i42_wat_req_k", "i43_wat_req_k", "pm_wat_req_k",
                                format = "first_found")[, , user$kli]
    ovm_prod_cell    <- readGDX(gdx, "ov_prod", "ovm_prod",
                                format = "first_found")[, , "level"][, , user$kli]
    ovm_prod_cell    <- setNames(ovm_prod_cell,
                                 gsub(".level", "", getNames(ovm_prod_cell), fixed = TRUE))

    # harmonize temporal dimension
    i_wat_req_k_cell <- i_wat_req_k_cell[, getItems(ovm_prod_cell, dim = 2), ]

    out$kli          <- as.magpie(ovm_prod_cell * i_wat_req_k_cell)
  }


  outout <- NULL
  for (i in 1:length(out)) {
    if (is.null(outout)) {
      outout <- out[[i]]
    } else {
      outout <- mbind(outout, out[[i]])
    }
  }

  # Transform water withdrawals to water consumption
  if (abstractiontype == "consumption") {

    # Crop water demand: roughly half of irrigation water withdrawals are returned to
    # the environment according to Jaegermeyr et al. (2015)
    # There is no estimate for the share of water that is consumed in livestock
    # water demand. Since drinking in sanitation water for livestock is rather
    # negligible compared to crop water demand, we assume the same share (50%)
    outout <- outout * 0.5

    # For non-agricultural water abstractions, both withdrawal and consumption are given
    # in the exogenous scenario.
    if (any(grepl(pattern = 'domestic|manufacturing|electricity', x = getItems(outout, dim = 3)))) {
      i42_watdem_total_c <- collapseNames(i42_watdem_total[, , abstractiontype])
      selectedSector   <- intersect(getItems(outout, dim = 3), getItems(i42_watdem_total_c, dim = 3))
      outout[, , selectedSector] <- i42_watdem_total_c[, getItems(outout, dim = 2), selectedSector] * ratioGrperTotal
    }
  }

  if (sum == TRUE) {
    if (users == "sectors") {
      # Summing over high level sectors for water use
      # i.e., agriculture, industry, manufacturing, livestock and ecosystems
      sectors <- out$sectors
      sectors <- rowSums(sectors, dims = 2)
      outout  <- sectors
    }
  }

  # disaggregate using croparea as weight
  outout <- gdxAggregate(gdx = gdx, x = outout,
                         weight = "land", types = "crop",
                         to = level, absolute = TRUE, dir = dir)
  # Limitation: croparea used as weight for all water usage types (also non-agricultural water uses)

  # convert from mio m^3 to km^3
  outout <- outout / 1000
  outout <- round(outout, digits)
  out(outout, file)

}
