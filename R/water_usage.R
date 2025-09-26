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
                        digits = 4) {

  sectors <- readGDX(gdx, "wat_dem")
  kcr     <- readGDX(gdx, "kcr")
  kli     <- readGDX(gdx, "kli")

  i42_watdem_total <- readGDX(gdx, "i42_watdem_total", react = "silent")
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
    usersInput <- users
    users <- NULL

    if ("kcr" %in% usersInput) {
      users <- c(users, kcr)
    } else if ("kli" %in% usersInput) {
      users <- c(users, kli)
    } else if ("sectors" %in% usersInput) {
      users <- c(users, sectors)
    } else {
      users <- usersInput
    }
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

  # Crop water use
  if (length(user$crops) > 0) {
    i_wat_req_k_cell <- readGDX(gdx, "i42_wat_req_k", "i43_wat_req_k", "pm_wat_req_k",
                                format = "first_found")[, , user$crops]

    if (abstractiontype == "consumption") {
      # Crop water demand: roughly half of irrigation water withdrawals are returned to
      # the environment according to Jaegermeyr et al. (2015)
      i_wat_req_k_cell <- i_wat_req_k_cell * 0.5
    }

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

  # Livestock water use
  if (length(user$kli) > 0) {

    i_wat_req_k_cell <- readGDX(gdx, "i42_wat_req_k", "i43_wat_req_k", "pm_wat_req_k",
                                format = "first_found")[, , user$kli]

    if (abstractiontype == "consumption") {
      # According to Wisser et al. (2024), 80% of livestock drinking water withrawal is consumptive.
      i_wat_req_k_cell <- i_wat_req_k_cell * 0.8
    }

    ovm_prod_cell    <- readGDX(gdx, "ov_prod", "ovm_prod",
                                format = "first_found")[, , "level"][, , user$kli]
    ovm_prod_cell    <- setNames(ovm_prod_cell,
                                 gsub(".level", "", getNames(ovm_prod_cell), fixed = TRUE))

    # harmonize temporal dimension
    i_wat_req_k_cell <- i_wat_req_k_cell[, getItems(ovm_prod_cell, dim = 2), ]

    out$kli          <- as.magpie(ovm_prod_cell * i_wat_req_k_cell)
  }

  # Sectoral water use
  if (length(user$sectors) > 0) {
    out$sectors <- readGDX(gdx, "ov_watdem", "ovm_watdem",
                           format = "first_found")[, , "level"][, , user$sectors]
    out$sectors <- setNames(out$sectors,
                            gsub(".level", "", getNames(out$sectors), fixed = TRUE))

    # Non-agricultural water usage is reported for the entire year
    # (not only the growing period as for agricultural water usage)
    if (any(grepl(pattern = 'domestic|manufacturing|electricity', x = user$sectors))) {

      # extract total water abstraction
      i42_watdem_total_ww <- collapseNames(i42_watdem_total[, , abstractiontype])
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

      if (any(round(ratioGrperTotal, digits = 6) > 1)) {
        stop("More water in growing period than in entire year.
             Please double-check starting from magpie4::water_usage()")
      }
    }

    if (any(grepl(pattern = "agriculture", x = user$sectors))) {

      if (is.null(out$kli)) {
        # water usage (in km3) convert to mio. m^3
        kliOUT <- water_usage(gdx = gdx, file = NULL, level = "cell", users = "kli",
                              sum = FALSE, seasonality = seasonality, abstractiontype = abstractiontype,
                              digits = 16) * 1000
      } else {
        kliOUT <- out$kli
      }
      if (is.null(out$kcr)) {
        # water usage (in km3) convert to mio. m^3
        kcrOUT <- water_usage(gdx = gdx, file = NULL, level = "cell", users = "kcr",
                              sum = FALSE, seasonality = seasonality, abstractiontype = abstractiontype,
                              digits = 16) * 1000
      } else {
        kcrOUT <- out$kcr
      }
      # Agriculture is the sum of crop and livestock water usage
      selectedSector <- intersect(user$sectors, "agriculture")
      out$sectors[, , selectedSector] <- dimSums(kliOUT, dim = 3) + dimSums(kcrOUT, dim = 3)
    }
  }

  outout <- NULL
  for (i in 1:length(out)) {
    if (is.null(outout)) {
      outout <- out[[i]]
    } else {
      outout <- mbind(outout, out[[i]])
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
                         to = level, absolute = TRUE)
  # Limitation: croparea used as weight for all water usage types (also non-agricultural water uses)

  # convert from mio m^3 to km^3
  outout <- outout / 1000
  outout <- round(outout, digits)
  return(outout)

}
