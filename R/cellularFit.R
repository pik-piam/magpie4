#' @title cellular fit
#' @description cellular fit/error/bias calculations at regional and global level
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level level at which the regional and global bias should be calculated. Options "cell" or "grid"
#' @param statistic R2, MAE, MPE (mean percentage error - bias), MAPE (mean absolute percentage error)
#' @param variable variable to be evaulated: land (land types) or crop (crop types)
#' @param dataset dataset to compare with. LUH3 only option for variable land. LUH3 and MAPSPAM for the crop variable.
#' @param water_aggr if irrigation types for crops should be agregated or not
#' @return returns selected statistic at regglo level for the historical part of the time horizon
#' @author Edna J. Molina Bacca, Patrick v. Jeetze
#' @importFrom magclass getYears getNames dimOrder read.magpie magpiesort
#' @importFrom madrat toolAggregate
#' @importFrom stats cor
#' @importFrom dplyr group_by summarize mutate select rename inner_join
#' @examples
#'
#'   \dontrun{
#'     x <- cellularFit(gdx)
#'   }
#'

cellularFit <- function(gdx, file = NULL, level = "cell", statistic = "MAE", variable = "land", dataset = "LUH3", water_aggr = FALSE) {

  # First Checks
  if (!level %in% c("cell", "grid")) {
    stop("Level must be either 'cell' or 'grid'")
  }
  if (variable == "land" && dataset != "LUH3") {
    stop("At the moment, `land` can only be compared to the `LUH3` dataset")
  }

  if (!file.exists(paste0(dirname(normalizePath(gdx)), "/LUH3_croparea_0.5.mz"))) {
    stop(
      "Cell validation is not possible. LUH3_croparea_0.5.mz and MAPSPAM_croparea_0.5.mz files are missing"
    )
  }

  mapFile <- Sys.glob(file.path(dirname(normalizePath(gdx)), "clustermap_*.rds"))
  mapping <- readRDS(mapFile)

  #Reads magpie output variable
  magpie <- cellularFitReadMagpie(gdx, variable, level, water_aggr)
  historical <- cellularFitReadHistorical(gdx, dataset, variable, level, water_aggr, mapping)

  getNames(magpie) <- gsub(".\\irrigated", "_irrigated", getNames(magpie))
  getNames(historical) <- gsub(".\\irrigated", "_irrigated", getNames(historical))

  # Intersect similar dimensions and subset objects
  names <- intersect(getNames(magpie), getNames(historical))
  years <- intersect(getYears(magpie, as.integer = TRUE),
                     getYears(historical, as.integer = TRUE))
  years <- years[years <= 2020]
  magpie <- magpie[, years, names]
  historical <- historical[, years, names]

  # Harmonize dim names
  getSets(magpie, fulldim = FALSE)[[2]] <- "year"
  getSets(historical, fulldim = FALSE)[[2]] <- "year"
  getSets(magpie, fulldim = FALSE)[[3]] <- "landuse"
  getSets(historical, fulldim = FALSE)[[3]] <- "landuse"

  # Helper function for conversion to dataframe
  .asDataFrame <- function(magpie) {
    return(rename(magclass::as.data.frame(magpie, rev = 3),
                  dplyr::all_of(c(Value = ".value"))))
  }

  # Merge either based on cell+region or based on x+y+region
  if (level == "cell") {
    getSets(magpie, fulldim = FALSE)[[1]] <- "region.cell"
    getSets(historical, fulldim = FALSE)[[1]] <- "region.cell"

    merged <- inner_join(
      .asDataFrame(magpie),
      .asDataFrame(historical),
      by = c("region", "cell", "year", "landuse"),
      suffix = c("_magpie", "_historical")
    )
  } else if (level == "grid") {
    coordToRegionMap <- mapping[, "region"]
    names(coordToRegionMap) <- mapping[, "cell"]
    magpie <- setItems(magpie, 1.3, unname(coordToRegionMap[getItems(magpie, 1)]))
    historical <- setItems(historical, 1.3, unname(coordToRegionMap[getItems(historical, 1)]))
    getSets(magpie, fulldim = FALSE)[[1]] <- "x.y.region"
    getSets(historical, fulldim = FALSE)[[1]] <- "x.y.region"

    magpie <- .asDataFrame(magpie)
    historical <- .asDataFrame(historical)
    if (any(as.character(unique(historical$region)) != as.character(unique(magpie$region)))) {
      stop("The regions of the MAgPIE output and historical data do not match")
    }

    merged <- inner_join(
      magpie,
      historical,
      by = c("x", "y", "region", "year", "landuse"),
      suffix = c("_magpie", "_historical")
    )
  }

  merged <- dplyr::select(merged, dplyr::all_of(c("region", "year", "landuse", "Value_magpie", "Value_historical")))

  # Calculate regional statistics using grouped data frames
  out <- merged |>
    group_by(dplyr::across(dplyr::all_of(c("region", "year", "landuse")))) |>
    summarize(Value = cellularFitCalcStat(.data$Value_magpie, .data$Value_historical, statistic),
                     .groups = "drop") |>
    rename(Region = .data$region, Year = .data$year, Data1 = .data$landuse) |>
    as.data.frame()

  # Calculate global statistics using grouped data frames
  outGlo <- merged |>
    group_by(dplyr::across(dplyr::all_of(c("year", "landuse")))) |>
    summarize(Value = cellularFitCalcStat(.data$Value_magpie, .data$Value_historical, statistic),
              .groups = "drop") |>
    mutate(Region = "GLO") |>
    rename(Year = .data$year, Data1 = .data$landuse) |>
    select(dplyr::all_of(c("Region", "Year", "Data1", "Value"))) |>
    as.data.frame()

  out <- rbind(out, outGlo)
  out <- as.magpie(out)
  if (water_aggr) {
    getNames(out) <- gsub("_irrigated", ".\\irrigated", getNames(out))
  }

  out(out, file)
}

cellularFitMapToLUH3 <- function(historical, gdx) {
  magpie2luh3 <- data.frame(matrix(nrow = 4, ncol = 2))
  names(magpie2luh3) <- c("MAgPIE", "LUH3")
  magpie2luh3[1, ] <- c("crop", "crop")
  magpie2luh3[4, ] <- c("urban", "urban")
  magpie2luh3[5, ] <- c("primforest", "primforest")
  magpie2luh3[6, ] <- c("secdforest", "secdforest")
  magpie2luh3[7, ] <- c("forestry", "forestry")
  magpie2luh3[8, ] <- c("other", "primother")
  magpie2luh3[9, ] <- c("other", "secdother")

  cfg <- gms::loadConfig(file.path(dirname(normalizePath(gdx)), "config.yml"))
  ### Make sure grassland types are consistent with 31_past realisation
  magpie2luh3[2, ] <- c("past", "past")
  if (grepl("grass", cfg$gms$past)) {
    magpie2luh3[3, ] <- c("range", "range")
  } else {
    magpie2luh3[3, ] <- c("past", "range")
  }

  return(madrat::toolAggregate(historical, magpie2luh3, from = "LUH3", to = "MAgPIE", dim = 3.1))
}

cellularFitCalcStat <- function(magpieVal, historicalVal, statistic) {
  if (statistic == "R2") {
    round(suppressWarnings(cor(magpieVal, historicalVal))^2, 3)
  } else if (statistic == "MAE") {
    absolute <- abs(historicalVal - magpieVal)
    sum(absolute) / length(absolute)
  } else if (statistic == "MPE") {
    relativeError <- (magpieVal - historicalVal) / historicalVal
    relativeError[!is.finite(relativeError)] <- NA
    round(sum(relativeError * 100, na.rm = TRUE) / length(relativeError[is.finite(relativeError)]),
          3)
  } else if (statistic == "MAPE") {
    absoluteError <- abs((magpieVal - historicalVal) / historicalVal)
    absoluteError[!is.finite(absoluteError)] <- NA
    round(sum(absoluteError * 100, na.rm = TRUE) / length(absoluteError[is.finite(absoluteError)]),
          3)
  }
}

cellularFitReadMagpie <- function(gdx, variable, level, water_aggr) {
  if (variable == "land") {
    return(land(gdx, file = NULL, level = level, sum = FALSE))
  } else if (variable == "crop") {
    return(croparea(gdx, file = NULL, level = level, products = "kcr", product_aggr = FALSE, water_aggr = water_aggr))
  }
}

cellularFitReadHistorical <- function(gdx, dataset, variable, level, water_aggr, mapping) {
  #Reads the historical data set to compare the magpie object with based on variable, resolution, and dataset
  if (dataset == "LUH3") {
    if (level == "cell" && variable == "land") {
      historical <- readGDX(gdx, "f10_land")
    } else if (level == "grid" && variable == "land") {
      historical <- read.magpie(paste0(dirname(normalizePath(gdx)), "/avl_land_full_t_0.5.mz"))
    } else if (level == "cell" && variable == "crop") {
      historical <- readGDX(gdx, "fm_croparea")
    } else if (level == "grid" && variable == "crop") {
      historical <- read.magpie(paste0(dirname(normalizePath(gdx)), "/LUH3_croparea_0.5.mz"))
    }
  } else if (dataset == "MAPSPAM") {
    historical <- read.magpie(paste0(dirname(normalizePath(gdx)), "/MAPSPAM_croparea_0.5.mz"))
    if (level == "cell") {
      historical <- magpiesort(toolAggregate(historical, rel = mapping, from = "cell", to = "cluster"))
    }
  }

  if (level == "grid" && variable == "land") {
    historical <- cellularFitMapToLUH3(historical, gdx)
  }

  # LUH3 crop types and irrigation regime dimensions are switched
  if (variable == "crop" && dataset == "LUH3") {
    historical <- dimOrder((historical), perm = c(2, 1), dim = 3)
  }

  if (water_aggr) {
    historical <- dimSums(historical, dim = 3.2)
  }

  return(historical)
}
