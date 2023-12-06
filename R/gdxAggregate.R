#' @title gdxAggregate
#' @description aggregates and disaggregates on spatial scales using mappings from the gdx files. Very specific to MAgPIE.
#'
#' @param gdx gdx file
#' @param x object to be aggrgeagted or disaggregated
#' @param weight weight can be either an object or a functionname in "", where the function provides the weight
#' @param to options: grid, cell, iso, reg, glo, regglo
#' @param absolute is it a absolute or a relative value (absolute: tons, relative: tons per hectare)
#' @param dir for gridded outputs: magpie output directory which containts the spamfiles or alternatively clusterspam*.rds
#' files for disaggregation.
#' @param ... further parameters handed on to weight function.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Edna J. Molina Bacca, Florian Humpenoeder
#' @examples
#' \dontrun{
#' gdp_pc <- income(gdx, level = "reg")
#' is.function(population)
#' gdp_pc_iso <- gdxAggregate(gdx = gdx, x = gdp_pc, weight = "population", to = "iso",
#'                            absolute = FALSE)
#' gdp_pc_glo <- gdxAggregate(gdx = gdx, x = gdp_pc, weight = "population", to = "glo",
#'                            absolute = FALSE)
#' gdp <- income(gdx, level = "reg", per_capita = FALSE)
#' gdp_iso <- gdxAggregate(gdx = gdx, x = gdp, weight = "population", to = "iso", absolute = TRUE)
#' gdp_glo <- gdxAggregate(gdx = gdx, x = gdp, weight = "population", to = "glo", absolute = TRUE)
#' }
#' @export
#' @importFrom magclass getSets setItems
#' @importFrom madrat toolAggregate
#' @importFrom magpiesets Cell2Country
#' @importFrom spam triplet
#' @importFrom luscale read.spam

gdxAggregate <- function(gdx, x, weight = NULL, to, absolute = TRUE, dir = ".", ...) {

  if (is.function(weight)) {
    warning("You provide a function as weight.
            It is better to use the functionname in ''
            to avoid overlapping naming in the R environment")
  }

  if (length(weight == 1)) {
    if (is.character(weight)) {
      weight <- get(weight, mode = "function")
    }
  }

  if (to == "GLO") {
    to <- "glo"
  }

  if (to == "REGGLO") {
    to <- "regglo"
  }


  reg_to_iso  <- readGDX(gdx = gdx, "i_to_iso")
  names(reg_to_iso)  <- c("reg", "iso")
  reg_to_cell <- readGDX(gdx = gdx, "cell")
  names(reg_to_cell) <- c("reg", "cell")
  reg_to_cell$cell   <- gsub(reg_to_cell$cell, pattern = "_", replacement = ".")

  # 0.5 grid mapping
  clustermap_filepath <- Sys.glob(file.path(dir, "clustermap*.rds"))
  spamfile            <- Sys.glob(file.path(dir, "*_sum.spam"))

  if (length(clustermap_filepath) == 1) {
    grid_to_cell           <- readRDS(clustermap_filepath)
    colnames(grid_to_cell) <- c("grid", "cell", "reg", "iso", "glo")
  } else if (length(spamfile == 1)) {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
    map_grid_iso <- readRDS(mapfile)
    grid_to_cell <- triplet(read.spam(spamfile))$indices
    grid_to_cell <- grid_to_cell[order(grid_to_cell[, 2]), 1]
    grid_to_cell <- reg_to_cell[match(x = grid_to_cell, table = as.integer(substring(reg_to_cell[, 2], 5, 7))), ]
    grid_to_cell$grid <- paste0(grid_to_cell[, 1], ".", 1:dim(grid_to_cell)[1])
    grid_to_cell <- cbind(map_grid_iso$grid, grid_to_cell$cell, grid_to_cell$reg, map_grid_iso$iso, map_grid_iso$glo)
    grid_to_cell <- as.data.frame(grid_to_cell)
    colnames(grid_to_cell) <- c("grid", "cell", "reg", "iso", "glo")
  } else {
    grid_to_cell <- NULL
  }



  if (all(dimnames(x)[[1]] %in% reg_to_cell$cell)) {
    from <- "cell"
  } else if (all(dimnames(x)[[1]] %in% grid_to_cell$grid)) {
    from <- "grid"
  } else {
    if (all(dimnames(x)[[1]] %in% reg_to_iso$iso)) {
      from <- "iso"
    } else if (all(dimnames(x)[[1]] %in% reg_to_iso$reg)) {
      from <- "reg"
    } else if (all(dimnames(x)[[1]] %in% c("GLO", "GLO.1"))) {
      from <- "glo"
    } else if (all(dimnames(x)[[1]] %in% c("GLO", "glo", reg_to_iso$reg))) {
      from <- "regglo"
    } else if (all(dimnames(x)[[1]] %in% c(grid_to_cell$grid))) {
      from <- "grid"
    } else {
      stop("unknown regions, wrong or missing dir")
    }
  }

  # otherwise it would lead to a second weight for the weight function
  if (from == "cell" & to == "iso" & absolute == FALSE & is.function(weight)) {
    stop("Weight for iso aggregation of a relative object must be an object at iso level.
         Run gdxAggregate to get the weight at iso level")
  }


  # get rid of unnecessary data
  if (from %in% c("REGGLO", "regglo")) {
    x <- x["GLO", , , invert = TRUE]
    if (!is.function(weight)) {
      if ("GLO" %in% getRegions(weight)) {
        if (length(getRegions(weight)) > 1) {
          weight <- weight["GLO", , , invert = TRUE]
        }
      }
    }
    from <- "reg"
  }


  if (to %in% c("regglo")) {
    to2 <- "regglo"
    to  <- "reg"
  } else {
    to2 <- FALSE
  }

  # no aggregation needed?
  if (from == to) {
    out <- x
    # cat(" no aggregation needed")
  } else {
    # cat(paste0("mapping: ",from,"_",to))
    # select mapping
    if ((from == "cell" & to == "iso") | (from == "iso" & to == "cell") | (from == "grid" & to == "iso") | (from == "iso" & to == "grid")) {
      # mappings for the disaggregation/aggregation process
      mapping     <- grid_to_cell

    } else if ((from == "iso" & to == "reg") | (from == "reg" & to == "iso")) {
      mapping <- reg_to_iso
    } else if ((from == "cell" & to == "reg") | (from == "reg" & to == "cell")) {
      mapping <- reg_to_cell
    } else if (to %in% c("glo")) {
      mapping <- data.frame(from = dimnames(x)[[1]],
                            glo = "GLO")
      names(mapping)[1] <- from
    } else if (((from == "grid") & (to == "cell")) | (((from == "cell") & (to == "grid"))) | (((from == "reg") & (to == "grid"))) | (((from == "grid") & (to == "reg")))) {
      mapping <- grid_to_cell
    } else if (from == "glo" & to == "iso") {
      mapping <- reg_to_iso
      mapping$glo <- "GLO"
      mapping <- mapping[, c("glo", "iso")]
    } else {
      stop("unknown mapping")
    }

    if (absolute == TRUE) {
      # weight only necessary for aggregation
      if (!is.function(weight)) {

        if (paste0(from, to) %in% c("gridcell", "gridiso", "gridreg", "gridglo", "cellreg", "cellglo", "isoreg", "isoglo", "regglo")) {
          # aggregation of absolute values needs no weight
          if (!is.null(weight)) {
            stop("weight provided, but aggregation of absolute values needs no weight")
          }
        } else if (paste0(from, to) %in% c("celliso")) {

          if (is.null(weight)) {
            stop("weight to dissagregate cell to grid is needed to be able to aggregate to iso level absolute values")
          }

        } else {
          # disaggregation of absolute values needs weight
          if (is.null(weight)) {
            stop("no weight provided, but disaggregation of absolute values needs weight")
          }
        }
      } else {

        if (paste0(from, to) %in% c("gridcell", "gridiso", "gridreg", "gridglo", "cellreg", "cellglo", "isoreg", "isoglo", "regglo")) {
          # aggregation of absolute values needs no weight
          weight <- NULL

        } else if (paste0(from, to) %in% c("celliso")) {
          weight <- weight(gdx = gdx, level = "grid", dir = dir, ...)
        } else {
          # disaggregation of absolute values needs weight
          weight <- weight(gdx = gdx, level = to, dir = dir, ...)
        }
      }
    } else if (absolute == FALSE) {
      if (!is.function(weight)) {
        if (paste0(from, to) %in% c("gridcell", "gridiso", "gridreg", "gridglo", "celliso", "cellreg", "cellglo", "isoreg", "isoglo", "regglo")) {
          # aggregation of relative values needs weight
          if (is.null(weight)) {
            stop("weight not provided, but aggregation of relative values needs weight")
          }
        } else {
          # disaggregation of relative values needs no weight
          if (!is.null(weight)) {
            stop("weight provided, but aggregation needs no weight")
          }
        }
      } else {

        if (paste0(from, to) %in% c("gridcell", "gridiso", "gridreg", "gridglo", "cellreg", "cellglo", "isoreg", "isoglo", "regglo")) {
          # aggregation of relative values needs weight
          weight <- weight(gdx = gdx, level = from, dir = dir, ...)
        } else if (paste0(from, to) %in% c("celliso")) {
          stop("Weight for celliso aggregation must be an object at iso level, function weight not supported")
        } else {
          # disaggregation of relative values needs no weight
          weight <- NULL
        }
      }
    } else {
      stop("absolute has to be binary")
    }

    if (!is.null(weight) && !is.null(getYears(weight))) { # problems can occur if function provides different years than object has
      if (!is.null(getYears(x))) {
        weight <- weight[, getYears(x), ]
      } else {
        weight <- weight[, 1, ]
        getYears(weight) <- NULL
      }
    }

    # add small number to treat weights of 0
    if (!is.null(weight)) {
      weight <- weight + 1e-16
    }

    if (from == "cell" & to == "iso") {

      if (absolute) {

        ind <- toolAggregate(x = x,   rel = mapping, weight = weight, from = "cell", to = "grid")
        out <- toolAggregate(x = ind, rel = mapping, weight = NULL,   from = "grid", to = "iso")

      } else {

        ind <- toolAggregate(x = x,   rel = mapping, weight = NULL,   from = "cell", to = "grid")
        out <- toolAggregate(x = ind, rel = mapping, weight = weight, from = "grid", to = "iso")

      }

    } else {

      if (((from == "iso" & to == "cell") | (from == "iso" & to == "grid")) & length(getItems(x, dim = 1)) != length(unique(mapping$iso))) {
        x1     <- x
        x      <- x[unique(mapping$iso), , ]
        #weight <- weight[unique(mapping$iso), ,]
        warning(paste0(round(max((dimSums(x1, dim = 1) - dimSums(x, dim = 1)) / dimSums(x1, dim = 1) * 100), digits = 4),
                       " % of the original data (x) is lost in aggregation"))
      }

      out <- toolAggregate(x = x, rel = mapping, weight = weight,
                           from = from, to = to, dim = 1)
      if (!is.null(weight)) {
        # aggregate weight as well (for the case it's needed again in regglo)
        weight <- toolAggregate(x = weight, rel = mapping,
                                from = from, to = to, dim = 1)
      }
    }
  }


  if (to2 == "regglo") {
    if (absolute == TRUE) {
      out <- mbind(out, setItems(dimSums(out, dim = 1), dim = 1, "GLO"))
    } else {
      if (is.function(weight)) {
        weight <- weight(gdx = gdx, level = "reg", dir = dir, ...)
      }
      out <- mbind(out,
                   setItems(dimSums(out * collapseNames(weight[getRegions(out), , ]), dim = 1) /
                              dimSums(collapseNames(weight[getRegions(out), , ]), dim = 1), dim = 1, "GLO")
      )
    }
  }

  # checks if aggregation to global level  of absolute values is the same for the input x and for the output out
  # commented out until dimSums(x,dim=1) = 0 sorted out
  # if(absolute==TRUE){
  #   if(any(abs(dimSums(x,dim=1)-(dimSums(out,dim=1)))/dimSums(x,dim=1)>1e-2)){
  #     warning("Global summation of input different than output")
  #   }
  # }

  return(out)

}
