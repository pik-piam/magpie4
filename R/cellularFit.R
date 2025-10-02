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
#' @examples
#'
#'   \dontrun{
#'     x <- cellularFit(gdx)
#'   }
#'

cellularFit <- function(gdx, file=NULL, level="cell", statistic="MAE",variable="land",dataset="LUH3",water_aggr =FALSE){

  # First Checks
  if(!level %in% c("cell", "grid")) stop("Level must be either 'cell' or 'grid'")
  if(variable=="Land" && dataset!="LUH3") stop("At the moment, `land` can only be compared to the `LUH3` dataset")

  if (!file.exists(paste0(dirname(normalizePath(gdx)), "/LUH3_croparea_0.5.mz"))){
    stop("Cell validation is not possible. LUH3_croparea_0.5.mz and MAPSPAM_croparea_0.5.mz files are missing")
  }

  map_file <- Sys.glob(file.path(dirname(normalizePath(gdx)), "clustermap_*.rds"))
  mapping <- readRDS(map_file)

  #Reads magpie output variable
  magpie <- if(variable=="land") land(gdx, file = NULL, level = level, sum = FALSE) else if(variable=="crop")
    croparea(gdx, file = NULL, level = level, products = "kcr",
             product_aggr = FALSE, water_aggr = water_aggr)


  #Reads the historical data set to compare the magpie object with based on variable, resolution, and dataset
  if(dataset=="LUH3"){

    if (level == "grid") cells <- if (length(getCells(variable)) == 59199) "magpiecell" else if (length(getCells(variable)) == 67420) "lpjcell"
    historical <- if (level == "cell" & variable=="land") readGDX(gdx, "f10_land") else
      if (level == "grid" & variable=="land") read.magpie(paste0(dirname(normalizePath(gdx)), "/avl_land_full_t_0.5.mz")) else
        if (level == "cell" & variable == "crop") readGDX(gdx, "fm_croparea") else
          if (level == "grid" & variable == "crop") read.magpie(paste0(dirname(normalizePath(gdx)), "/LUH3_croparea_0.5.mz"))

  }else if(dataset=="MAPSPAM"){
    historical <- read.magpie(paste0(dirname(normalizePath(gdx)), "/MAPSPAM_croparea_0.5.mz"))
    if(level=="cell"){
      historical <- magpiesort(toolAggregate(historical, rel = mapping, from = "cell", to = "cluster"))
    }

  }

  if (level == "grid" & variable == "land") {
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
    if (grepl("grass", cfg$gms$past)) {
      magpie2luh3[3, ] <- c("range", "range")
      magpie2luh3[2, ] <- c("past", "past")
    } else {
      magpie2luh3[3, ] <- c("past", "range")
      magpie2luh3[2, ] <- c("past", "past")
    }

    historical <- madrat::toolAggregate(historical, magpie2luh3, from = "LUH3", to = "MAgPIE", dim = 3.1)
  }

  # LUH3 crop types and irrigation regime dimensions are switched
  if (variable == "crop" & dataset=="LUH3") historical <- dimOrder((historical), perm = c(2, 1), dim = 3)

  historical <- if(water_aggr) dimSums(historical,dim=3.2) else historical
  getNames(magpie) <- gsub(".\\irrigated", "_irrigated", getNames(magpie))
  getNames(historical) <- gsub(".\\irrigated", "_irrigated", getNames(historical))

  # Intersect similar dimensions
  names <- intersect(getNames(magpie),getNames(historical))
  years <- intersect(getYears(magpie, as.integer = TRUE), getYears(historical, as.integer = TRUE))
  years <- years[years<=2020]

  # Calculation of the fit/error/bias statistics

  if(level == "cell"){
    historical <- magclass::as.data.frame(historical, rev=1)
    magpie <- magclass::as.data.frame(magpie, rev=1)
  } else if (level == "grid") {
    historical <- setItems(historical, dim=1, mapping[,"region"])
    historical <- magclass::as.data.frame(historical, rev=1)
    magpie <- setItems(magpie, dim=1, mapping[,"region"])
    magpie <- magclass::as.data.frame(magpie, rev=1)
  }
  if(all(as.character(unique(historical$Region))!=as.character(unique(magpie$Region)))) stop(
    "The regions of the MAgPIE output and historical data do not match")

  #Final variables
  nrows <- length(as.character(unique(historical$Region))) * length(as.character(names)) * length(as.numeric(years))
  out <- data.frame(matrix(ncol = length(colnames(magpie)) - 2, nrow = nrows))
  colnames(out) <- colnames(magpie)[2:(length(colnames(magpie))-1)]
  outGlo <- data.frame(matrix(ncol = length(colnames(magpie)) - 2, nrow = length(as.character(names)) * length(as.numeric(years))))
  colnames(outGlo) <- colnames(magpie)[2:(length(colnames(magpie)) - 1)]

  aux <- 1
  aux1 <- 1

  for (n in as.character(names)){
    for(y in as.numeric(years)){
      for(r in as.character(unique(historical$Region))){

        magpieSub <- magpie[magpie$Region == r & magpie$Year == y & magpie$Data1 == n, ]
        historicalSub <- historical[historical$Region == r & historical$Year == y & historical$Data1 == n, ]

        if(statistic=="R2"){
          stat <- round(suppressWarnings(cor(magpieSub$Value, historicalSub$Value))^2, 3)
        } else if(statistic=="MAE"){
          absolute <- abs(historicalSub$Value - magpieSub$Value)
          stat <- sum(absolute)/length(absolute)
        }else if(statistic=="MPE"){
          relativeError <- (magpieSub$Value - historicalSub$Value) / historicalSub$Value
          relativeError[!is.finite(relativeError)] <- NA
          stat <- round(sum(relativeError * 100, na.rm = TRUE) / length(relativeError[is.finite(relativeError)]),3)
        }else if(statistic=="MAPE"){
          absoluteError <- abs((magpieSub$Value - historicalSub$Value) / historicalSub$Value)
          absoluteError[!is.finite(absoluteError)] <- NA
          stat <- round(sum(absoluteError * 100, na.rm = TRUE) / length(absoluteError[is.finite(absoluteError)]), 3)
        }

        out[aux, "Region"] <- r
        out[aux, "Year"] <- y
        out[aux, "Data1"] <- n
        out[aux, "Value"] <- stat

        aux <- aux + 1

      }

      #Calculation of global parameters
      magpieSub <- magpie[magpie$Year == y & magpie$Data1 == n, ]
      historicalSub <- historical[historical$Year == y & historical$Data1 == n, ]

      if (statistic == "R2") {
        stat <- round(suppressWarnings(cor(magpieSub$Value, historicalSub$Value))^2, 3)
      } else if (statistic == "MAE") {
        absolute <- abs(historicalSub$Value - magpieSub$Value)
        stat <- sum(absolute) / length(absolute)
      } else if (statistic == "MPE") {
        relativeError <- (magpieSub$Value - historicalSub$Value) / historicalSub$Value
        relativeError[!is.finite(relativeError)] <- NA
        stat <- round(sum(relativeError * 100, na.rm = TRUE) / length(relativeError[is.finite(relativeError)]), 3)
      } else if (statistic == "MAPE") {
        absoluteError <- abs((magpieSub$Value - historicalSub$Value) / historicalSub$Value)
        absoluteError[!is.finite(absoluteError)] <- NA
        stat <- round(sum(absoluteError * 100, na.rm = TRUE) / length(absoluteError[is.finite(absoluteError)]), 3)
      }

      outGlo[aux1, "Region"] <- "GLO"
      outGlo[aux1, "Year"] <- y
      outGlo[aux1, "Data1"] <- n
      outGlo[aux1, "Value"] <- stat

      aux1 <- aux1 + 1
    }
  }

  out <- rbind(out,outGlo)
  out <- as.magpie(out)
  if (water_aggr) getNames(out) <- gsub( "_irrigated",".\\irrigated", getNames(out))

  out(out,file)
}
