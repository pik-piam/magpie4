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
#' @param dataset dataset to compare with. LUH2 only option for variable land. LUH2 and MAPSPAM for the crop variable.
#' @details 
#' @return returns selected statistic at regglo level for the historical part of the time horizon
#' @author Edna J. Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luplot as.ggplot qualityMeasure
#' @importFrom magclass getYears getNames dimOrder read.magpie magpiesort 
#' @importFrom madrat toolAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- cellularFit(gdx)
#'   }
#'

cellularFit <- function(gdx, file=NULL, level="cell", statistic="MAE",variable="land",dataset="LUH2",water_aggr =FALSE){
  
  # First Checks
  if(!level %in% c("cell", "grid")) stop("Level must be either 'cell' or 'grid'")
  if(variable=="Land" && dataset!="LUH2") stop("At the moment, `land` can only be compared to the `LUH2` dataset")
  
  #Map file between different spatial resolution
  dir <- gsub("fulldata.gdx", "", gdx)
  map_file <- Sys.glob(file.path(dir, "clustermap_*.rds"))
  mapping <- readRDS(map_file)
  
  #Reads magpie output variable
  magpie <- if(variable=="land") land(gdx, file = NULL, level = level, sum = FALSE, dir = dir) else if(variable=="crop") 
              croparea(gdx, file = NULL, level = level, products = "kcr",
                     product_aggr = FALSE, water_aggr = water_aggr, dir = dir)

  
  #Reads the historical data set to compare the magpie object with based on variable, resolution, and dataset
  if(dataset=="LUH2"){

  if (level == "grid") cells <- if (length(getCells(variable)) == 59199) "magpiecell" else if (length(getCells(variable)) == 67420) "lpjcell"
  historical <- if (level == "cell" & variable=="land") readGDX(gdx, "f10_land") else 
                if (level == "grid" & variable=="land") read.magpie(paste0(dir, "/avl_land_full_t_0.5.mz")) else
                if (level == "cell" & variable == "crop") readGDX(gdx, "fm_croparea") else
                if (level == "grid" & variable == "crop") read.magpie(paste0(dir, "/LUH2_croparea_0.5.mz"))
  
  }else if(dataset=="MAPSPAM"){
    historical <- read.magpie(paste0(dir, "/MAPSPAM_croparea_0.5.mz"))
    if(level=="cell"){
    historical <- magpiesort(toolAggregate(historical, rel = mapping, from = "cell", to = "cluster")) 
   }

  }
  
   # LUH2 crop types and irrigation regime dimensions are switched
  if (variable == "crop" & dataset=="LUH2") historical <- dimOrder((historical), perm = c(2, 1), dim = 3)
 
  historical <- if(water_aggr) dimSums(historical,dim=3.2) else historical 
  getNames(magpie) <- gsub(".\\irrigated", "_irrigated", getNames(magpie))
  getNames(historical) <- gsub(".\\irrigated", "_irrigated", getNames(historical))

  # Intersect similar dimensions
  names <- intersect(getNames(magpie),getNames(historical))
  years <- intersect(getYears(magpie, as.integer = TRUE), getYears(historical, as.integer = TRUE))
  years <- years[years<=2020]
  
  # Calculation of the fit/error/bias statistics
  historical <- luplot::as.ggplot(historical)
  magpie<-luplot::as.ggplot(magpie)  
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
     data<-merge(magpieSub,historicalSub,by=c("Cell","Region","Year","Data1","Scenario"))
     
     if(statistic=="R2"){
     stat <- round(cor(data$Value.x, data$Value.y)^2, 3)
     } else if(statistic=="MAE"){
     stat <- round(luplot::qualityMeasure(pd = data$Value.x, od = data$Value.y, measures = "MAE", p_value = FALSE),3)
     }else if(statistic=="MPE"){
      relativeError <- (data$Value.x - data$Value.y) / data$Value.y
      relativeError[!is.finite(relativeError)] <- NA
      stat <- round(sum(relativeError * 100, na.rm = TRUE) / length(relativeError[is.finite(relativeError)]),3)
     }else if(statistic=="MAPE"){
      absoluteError <- abs((data$Value.x - data$Value.y) / data$Value.y)
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
   data <- merge(magpieSub, historicalSub, by = c("Cell", "Region", "Year", "Data1", "Scenario"))

   if (statistic == "R2") {
     stat <- round(cor(data$Value.x, data$Value.y)^2, 3)
   } else if (statistic == "MAE") {
     stat <-round(luplot::qualityMeasure(pd = data$Value.x, od = data$Value.y, measures = "MAE", p_value = FALSE),3)
   } else if (statistic == "bias") {
     relativeError <- (data$Value.x - data$Value.y) / data$Value.y
     relativeError[!is.finite(relativeError)] <- NA
     stat <- round(sum(relativeError * 100, na.rm = TRUE) / length(relativeError[is.finite(relativeError)]),3)
   } else if (statistic == "error") {
     absoluteError <- abs(data$Value.x - data$Value.y)
     stat <- round(sum(absoluteError) / sum(data$Value.y),3)
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
