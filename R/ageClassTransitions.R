#' @title ageClassTransitions
#' @description Flows of secondary forest, forestry (by type of module 32) and other land (othernat and
#' youngsecdf of module 35) by age class in each time step, by process:
#' \itemize{
#'   \item disturbance: area that is disturbed and regrows in the same land type (secondary forest,
#'   forestry);
#'   \item recovery: othernat that becomes youngsecdf; maturation: youngsecdf that becomes secondary forest;
#'   \item outflow: area that leaves the land type (reduction minus regrowth);
#'   \item regrowth: harvested area that regrows in the same land type (harvested secondary forest and
#'   othernat, replanted forestry; zero for youngsecdf, whose harvested area becomes othernat).
#' }
#' Replanted forestry is split over the age classes in proportion to the reduction.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, see \code{\link{gdxAggregate}}
#' @return Area in Mha per time step with names "process.land.type.ac" (type "total" for secondary forest)
#' @author Patrick v. Jeetze
#' @seealso \code{\link{ageClasses}}
#' @examples
#' \dontrun{
#' x <- ageClassTransitions(gdx)
#' }
ageClassTransitions <- function(gdx, file = NULL, level = "cell") {
  # Flows with the names "process.land.type.ac", from an object with items "ac" or "type.ac".
  .label <- function(x, process, land, type = NULL) {
    if (!is.null(type)) {
      x <- add_dimension(x, dim = 3.1, add = "type", nm = type)
    }
    getSets(x, fulldim = FALSE)[3] <- "type.ac"
    x <- add_dimension(x, dim = 3.1, add = "land", nm = land)
    return(add_dimension(x, dim = 3.1, add = "process", nm = process))
  }

  # Outflow (reduction minus regrowth) and regrowth of a land type.
  .outflowRegrowth <- function(reduction, regrowth, land, type = NULL) {
    return(mbind(.label(pmax(reduction - regrowth, 0), "outflow", land, type),
                 .label(regrowth, "regrowth", land, type)))
  }

  .lvl <- function(symbol) {
    return(readGDX(gdx, symbol, select = list(type = "level")))
  }

  flows <- list(.label(readGDX(gdx, "p35_disturbance_loss_secdf"), "disturbance", "secdforest", "total"))
  forestryDisturbance <- readGDX(gdx, "p32_disturbance_loss_ftype32", react = "silent")
  if (!is.null(forestryDisturbance)) {
    flows[[length(flows) + 1]] <- .label(forestryDisturbance, "disturbance", "forestry")
  }
  flows[[length(flows) + 1]] <- .label(readGDX(gdx, "p35_forest_recovery_area"), "recovery", "other", "othernat")
  flows[[length(flows) + 1]] <- .label(readGDX(gdx, "p35_maturesecdf"), "maturation", "other", "youngsecdf")

  # secondary forest and other land: harvested secondary forest and othernat regrow in their land type
  flows[[length(flows) + 1]] <- .outflowRegrowth(.lvl("ov35_secdforest_reduction"), .lvl("ov35_hvarea_secdforest"),
                                                 "secdforest", "total")
  otherReduction <- .lvl("ov35_other_reduction")
  otherHarvest <- .lvl("ov35_hvarea_other")
  flows[[length(flows) + 1]] <- .outflowRegrowth(otherReduction[, , "othernat"], otherHarvest[, , "othernat"],
                                                 "other")
  flows[[length(flows) + 1]] <- .outflowRegrowth(otherReduction[, , "youngsecdf"],
                                                 otherReduction[, , "youngsecdf"] * 0, "other")

  # forestry: the replanted area of each type split over the age classes in proportion to the reduction
  forestryReduction <- .lvl("ov32_land_reduction")
  replant <- .lvl("ov32_land_replant")
  forestryRegrowth <- forestryReduction * 0
  for (type in intersect(getItems(forestryReduction, dim = 3.1), getItems(replant, dim = 3))) {
    reduction <- as.array(collapseDim(forestryReduction[, , type], dim = 3.1))
    reductionSum <- rowSums(matrix(reduction, ncol = dim(reduction)[3]))
    replanted <- as.vector(as.array(replant[, getYears(forestryReduction), type]))
    forestryRegrowth[, , type] <- reduction * (replanted / ifelse(reductionSum > 0, reductionSum, 1))
  }
  flows[[length(flows) + 1]] <- .outflowRegrowth(forestryReduction, forestryRegrowth, "forestry")

  x <- gdxAggregate(gdx, mbind(flows), to = level, absolute = TRUE)
  out(x, file)
}
