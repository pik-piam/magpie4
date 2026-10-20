#' @title landTransitions
#' @description Land transitions of each time step by stage, in the order in which the model applies
#' them. Stages: disturbance (primary to secondary forest), recoveryForestry and recovery (othernat to
#' youngsecdf, from the forestry abandonment of the previous time step first), maturation (youngsecdf to
#' secondary forest), forestryPassThrough (area that enters forestry before it leaves), snvRelocation
#' (\code{p29_snv_relocation}, if the run has it), peatRewetting (reduction of managed land on drained peat,
#' peatland realisation v2) and optimisation. Without sub-types, the stages within a land type (recovery,
#' forestryPassThrough) are left out, and only \code{ov_lu_transitions} is required; stages whose symbols
#' the run lacks are left out.
#'
#' @details With sub-types, forestry is split into the types of module 32 and other land into othernat and
#' youngsecdf (module 35). The model gives the transitions between main types (\code{ov_lu_transitions})
#' and the outflow and inflow of each sub-type (expansion and reduction of the forestry types, reduction and
#' harvest of the other land types), but not their sources and destinations. The transitions between
#' sub-types are fitted to these margins by iterative proportional fitting; without further information all
#' sub-types of a main type get the same composition of sources and destinations. The SNV relocation and the
#' land reduction on drained peat are taken from the transitions of the optimisation in proportion to them.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, see \code{\link{gdxAggregate}}
#' @param subtypes TRUE to split forestry and other land into their sub-types, FALSE for the main types
#' @return Area in Mha per time step with names "stage.from.to" (transitions between different types)
#' @author Patrick v. Jeetze
#' @seealso \code{\link{reportLandTransitions}}, \code{\link{land}}
#' @examples
#' \dontrun{
#' x <- landTransitions(gdx)
#' }
landTransitions <- function(gdx, file = NULL, level = "cell", subtypes = TRUE) { # nolint: cyclocomp_linter
  .lvl <- function(symbol) {
    return(readGDX(gdx, symbol, select = list(type = "level")))
  }

  # Cluster x item matrix of one year of a magclass object.
  .yearMatrix <- function(x, year, clusters) {
    out <- matrix(x[, year, ], nrow = dim(x)[1], dimnames = list(getItems(x, dim = 1), getItems(x, dim = 3)))
    return(out[clusters, , drop = FALSE])
  }

  # Stage matrix (clusters x one transition) of cluster values.
  .stageMatrix <- function(values, transition, clusters) {
    return(matrix(as.vector(values), ncol = 1, dimnames = list(clusters, transition)))
  }

  # Cluster inflow into a main type, from the transitions between main types.
  .mainInflow <- function(mainTransitions, type, mainTypes) {
    return(rowSums(mainTransitions[, paste(setdiff(mainTypes, type), type, sep = "."), drop = FALSE]))
  }

  # Transitions between sub-types (clusters x "from.to") fitted by iterative proportional fitting to the
  # transitions between main types (blocks), the transitions within a main type (internal) and the outflow
  # and inflow of each sub-type; noExternal marks sub-types without outflow to other main types.
  .fitSubtypes <- function(mainTransitions, outflow, inflow, mainOf, allowed, internal, noExternal) {
    types <- names(mainOf)
    mainTypes <- unique(mainOf)
    clusters <- rownames(mainTransitions)
    outflow <- outflow[, types, drop = FALSE]
    inflow <- inflow[, types, drop = FALSE]

    .blockTotal <- function(fMain, kMain) {
      if (fMain == kMain) {
        return(internal[, fMain])
      }
      pair <- paste(fMain, kMain, sep = ".")
      if (pair %in% colnames(mainTransitions)) {
        return(mainTransitions[, pair])
      }
      return(numeric(length(clusters)))
    }

    # start values (clusters x from x to): 1 where the transition is allowed, its block has a positive
    # total and the source type has outflow to other main types
    y <- array(0, dim = c(length(clusters), length(types), length(types)), dimnames = list(clusters, types, types))
    pairs <- which(allowed[types, types, drop = FALSE], arr.ind = TRUE)
    for (i in seq_len(nrow(pairs))) {
      f <- types[pairs[i, 1]]
      k <- types[pairs[i, 2]]
      y[, f, k] <- as.numeric(.blockTotal(mainOf[f], mainOf[k]) > 0)
    }
    for (s in colnames(noExternal)) {
      y[noExternal[, s], s, mainOf != mainOf[s]] <- 0
    }

    deviation <- Inf
    for (iter in seq_len(1000)) {
      for (fMain in mainTypes) {
        for (kMain in mainTypes) {
          fSub <- types[mainOf == fMain]
          kSub <- types[mainOf == kMain]
          blockSum <- apply(y[, fSub, kSub, drop = FALSE], 1, sum)
          scale <- ifelse(blockSum > 0, .blockTotal(fMain, kMain) / blockSum, 0)
          y[, fSub, kSub] <- y[, fSub, kSub, drop = FALSE] * scale
        }
      }
      rowSum <- apply(y, c(1, 2), sum)
      y <- y * as.vector(ifelse(rowSum > 0, outflow / rowSum, 0))
      colSum <- apply(y, c(1, 3), sum)
      scale <- ifelse(colSum > 0, inflow / colSum, 0)
      for (k in types) {
        y[, , k] <- y[, , k] * scale[, k]
      }

      previousDeviation <- deviation
      deviation <- max(abs(apply(y, c(1, 2), sum) - outflow))
      if (deviation < 1e-7 || previousDeviation - deviation < 1e-12) {
        break
      }
    }
    for (fMain in mainTypes) {
      for (kMain in mainTypes) {
        blockSum <- apply(y[, types[mainOf == fMain], types[mainOf == kMain], drop = FALSE], 1, sum)
        deviation <- max(deviation, abs(blockSum - .blockTotal(fMain, kMain)))
      }
    }
    if (deviation >= 1e-6) {
      warning("sub-type split: margins deviate by ", signif(deviation, 3))
    }

    out <- matrix(0, length(clusters), nrow(pairs),
                  dimnames = list(clusters, paste(types[pairs[, 1]], types[pairs[, 2]], sep = ".")))
    for (i in seq_len(nrow(pairs))) {
      out[, i] <- y[, pairs[i, 1], pairs[i, 2]]
    }
    return(out)
  }

  # Transitions of the optimisation of year t between sub-types (clusters x "from.to"), with the area
  # passing through forestry (where the transitions into forestry exceed the expansion), split over the
  # forestry types in proportion to their area, as a stage of its own (passThrough).
  .optimisationTransitions <- function(t, mainTransitions, forestryArea) {
    # forestry types: expansion and reduction, plus the area passing through
    expansion <- .yearMatrix(forestryExpansion, t, clusters)[, forestryTypes, drop = FALSE]
    reduction <- .yearMatrix(forestryReduction, t, clusters)[, forestryTypes, drop = FALSE]
    reshuffle <- rowSums(expansion) - .mainInflow(mainTransitions, "forestry", mainTypes)
    passThrough <- pmax(-reshuffle, 0)

    forestrySum <- rowSums(forestryArea)
    areaShare <- forestryArea / ifelse(forestrySum > 0, forestrySum, 1)
    areaShare[forestrySum == 0, ] <- 1 / length(forestryTypes)
    expansion <- expansion + passThrough * areaShare
    reduction <- reduction + passThrough * areaShare

    # other land types: reduction and harvest; harvested youngsecdf regrows as othernat
    otherReductionT <- .yearMatrix(dimSums(otherReduction[, t, ], dim = "ac"), t, clusters)[, otherTypes, drop = FALSE]
    otherHarvestT <- .yearMatrix(dimSums(otherHarvest[, t, ], dim = "ac"), t, clusters)[, otherTypes, drop = FALSE]

    # outflow and inflow of each sub-type
    outflow <- matrix(0, length(clusters), length(types), dimnames = list(clusters, types))
    inflow <- outflow
    for (k in setdiff(types, c(forestryTypes, otherTypes))) {
      outflow[, k] <- rowSums(mainTransitions[, paste(k, setdiff(mainTypes, k), sep = "."), drop = FALSE])
      inflow[, k] <- .mainInflow(mainTransitions, k, mainTypes)
    }
    outflow[, forestryTypes] <- reduction
    inflow[, forestryTypes] <- expansion
    outflow[, "othernat"] <- otherReductionT[, "othernat"] - otherHarvestT[, "othernat"]
    outflow[, "youngsecdf"] <- otherReductionT[, "youngsecdf"]
    inflow[, "othernat"] <- .mainInflow(mainTransitions, "other", mainTypes) + otherHarvestT[, "youngsecdf"]
    inflow[, "youngsecdf"] <- 0
    outflow <- pmax(outflow, 0)

    # transitions between sub-types of one main type: changes of forestry type, harvest of youngsecdf
    internal <- matrix(0, length(clusters), length(mainTypes), dimnames = list(clusters, mainTypes))
    internal[, "forestry"] <- pmax(reshuffle, 0)
    internal[, "other"] <- otherHarvestT[, "youngsecdf"]

    # a forestry type can expand and reduce in the same cluster and year; the model nets the transitions
    # through forestry, so the split needs transitions from a type to itself
    splitAllowed <- allowed
    splitAllowed[cbind(forestryTypes, forestryTypes)] <- TRUE
    # other land types whose whole reduction is harvest (regrowing as othernat)
    noExternal <- (otherReductionT - otherHarvestT) < 1e-9
    optimisation <- .fitSubtypes(mainTransitions, outflow, inflow, mainOf, splitAllowed, internal, noExternal)

    # the share of the inflow to the forestry types that passes through forestry
    from <- sub("[.].*$", "", colnames(optimisation))
    to <- sub("^.*[.]", "", colnames(optimisation))
    intoForestry <- to %in% forestryTypes & !(from %in% forestryTypes)
    passShare <- passThrough * areaShare / ifelse(expansion > 0, expansion, 1)
    passIn <- optimisation[, intoForestry, drop = FALSE] * passShare[, to[intoForestry], drop = FALSE]
    optimisation[, intoForestry] <- optimisation[, intoForestry] - passIn

    return(list(optimisation = optimisation, passThrough = passIn))
  }

  # ---- symbols of the run ----------------------------------------------------------------------------

  transitions <- readGDX(gdx, "ov_lu_transitions", select = list(type = "level"), react = "silent")
  if (is.null(transitions)) {
    warning("Land transitions cannot be calculated as ov_lu_transitions could not be found in GDX file! ",
            "NULL is returned!")
    return(NULL)
  }
  disturbance <- readGDX(gdx, "p35_disturbance_loss_primf", react = "silent")
  maturation <- readGDX(gdx, "p35_maturesecdf", react = "silent")
  snvRelocation <- readGDX(gdx, "p29_snv_relocation", react = "silent")
  if (!is.null(disturbance)) {
    disturbance <- dimSums(disturbance, dim = 3)
  }
  if (!is.null(maturation)) {
    maturation <- dimSums(maturation, dim = 3)
  }

  clusters <- getItems(transitions, dim = 1)
  years <- getYears(transitions)
  mainTypes <- c("crop", "past", "forestry", "primforest", "secdforest", "urban", "other")
  mainOf <- stats::setNames(mainTypes, mainTypes)

  # sub-types: the forestry types of module 32 and the other land types of module 35, their flows, and the
  # transitions between sub-types that the model allows: those between main types with a positive upper
  # bound (modules/10_land/landmatrix_dec18/presolve.gms); youngsecdf receives area only in the presolve
  if (subtypes) {
    forestry <- .lvl("ov_land_forestry")
    forestryExpansion <- .lvl("ov_landexpansion_forestry")
    forestryReduction <- .lvl("ov_landreduction_forestry")
    otherReduction <- .lvl("ov35_other_reduction")
    otherHarvest <- .lvl("ov35_hvarea_other")
    recovery <- dimSums(readGDX(gdx, "p35_forest_recovery_area"), dim = 3)
    forestryTypes <- getItems(forestry, dim = 3)
    otherTypes <- getItems(.lvl("ov_land_other"), dim = "othertype35")
    mainOf <- c(crop = "crop", past = "past",
                stats::setNames(rep("forestry", length(forestryTypes)), forestryTypes),
                primforest = "primforest", secdforest = "secdforest", urban = "urban",
                stats::setNames(rep("other", length(otherTypes)), otherTypes))
    types <- names(mainOf)

    upper <- readGDX(gdx, "ov_lu_transitions", select = list(type = "upper"))
    mainAllowed <- apply(as.array(upper) > 0, 3, any)
    allowed <- outer(types, types, "!=") & matrix(mainAllowed[outer(mainOf, mainOf, paste, sep = ".")],
                                                  length(types))
    dimnames(allowed) <- list(types, types)
    allowed[, "youngsecdf"] <- FALSE

    # forestry abandonment of the previous year (0 in the first year)
    abandoned <- matrix(as.array(transitions)[clusters, , "forestry.other"], nrow = length(clusters))
    abandoned <- cbind(0, abandoned[, -length(years), drop = FALSE])
  }

  # decline of drained peat by land type (clusters x years x types), 0 in the first year, with peatland
  # realisation v2 (ov58_peatland by land58)
  peatLoss <- NULL
  peatlandLevel <- readGDX(gdx, "ov58_peatland", select = list(type = "level"), react = "silent")
  if (!is.null(peatlandLevel) && names(dimnames(peatlandLevel))[3] == "land58") {
    peatland <- PeatlandArea(gdx, level = "cell", sum = FALSE)
    drained <- intersect(getNames(peatland), c("crop", "past", "forestry"))
    drainedLr <- as.array(peatland)[clusters, years, drained, drop = FALSE]
    peatLoss <- pmax(drainedLr[, c(1, seq_along(years)[-length(years)]), , drop = FALSE] - drainedLr, 0)
    dimnames(peatLoss) <- dimnames(drainedLr)
  }

  # ---- stages of each time step ----------------------------------------------------------------------

  stages <- list()
  for (i in seq_along(years)) {
    t <- years[i]
    mainTransitions <- .yearMatrix(transitions, t, clusters)

    if (subtypes) {
      # transitions of the optimisation between sub-types, with the area passing through forestry as a
      # stage of its own
      forestryArea <- .yearMatrix(forestry, years[max(i - 1, 1)], clusters)[, forestryTypes, drop = FALSE]
      split <- .optimisationTransitions(t, mainTransitions, forestryArea)
      optimisation <- split[["optimisation"]]
    } else {
      pairs <- colnames(mainTransitions)
      optimisation <- mainTransitions[, sub("[.].*$", "", pairs) != sub("^.*[.]", "", pairs), drop = FALSE]
    }

    # SNV relocation (q29_land_snv_trans), taken from the transitions from cropland to secondary forest
    # and other land in proportion to them
    snvStage <- NULL
    if (!is.null(snvRelocation)) {
      toSnv <- c("crop.secdforest", if (subtypes) "crop.othernat" else "crop.other")
      available <- rowSums(optimisation[, toSnv, drop = FALSE])
      amount <- pmin(as.vector(snvRelocation[clusters, t, ]), available)
      snvStage <- optimisation[, toSnv, drop = FALSE] * (amount / ifelse(available > 0, available, 1))
      optimisation[, toSnv] <- pmax(optimisation[, toSnv] - snvStage, 0)
    }

    # land reduction on drained peat (q58_peatlandMan), taken from the transitions out of each managed land
    # type in proportion to them
    peatStage <- NULL
    if (!is.null(peatLoss)) {
      fromMain <- mainOf[sub("[.].*$", "", colnames(optimisation))]
      out <- fromMain %in% dimnames(peatLoss)[[3]] & fromMain != mainOf[sub("^.*[.]", "", colnames(optimisation))]
      available <- t(rowsum(t(optimisation[, out, drop = FALSE]), fromMain[out]))
      loss <- matrix(peatLoss[, t, colnames(available)], nrow(available))
      share <- pmin(available, loss) / ifelse(available > 0, available, 1)
      peatStage <- optimisation[, out, drop = FALSE] * share[, fromMain[out], drop = FALSE]
      optimisation[, out] <- optimisation[, out] - peatStage
      optimisation <- pmax(optimisation, 0)
      peatStage <- peatStage[, colSums(peatStage) > 0, drop = FALSE]
    }

    stages[[t]] <- list()
    if (!is.null(disturbance)) {
      stages[[t]][["disturbance"]] <- .stageMatrix(disturbance[clusters, t, ], "primforest.secdforest", clusters)
    }
    if (subtypes) {
      # recovery from the forestry abandonment of the previous step first, then from other othernat
      recoveryTotal <- as.vector(recovery[clusters, t, ])
      recoveryForestry <- pmin(recoveryTotal, abandoned[, i])
      stages[[t]][["recoveryForestry"]] <- .stageMatrix(recoveryForestry, "othernat.youngsecdf", clusters)
      stages[[t]][["recovery"]] <- .stageMatrix(recoveryTotal - recoveryForestry, "othernat.youngsecdf", clusters)
    }
    if (!is.null(maturation)) {
      stages[[t]][["maturation"]] <- .stageMatrix(maturation[clusters, t, ],
                                                  if (subtypes) "youngsecdf.secdforest" else "other.secdforest",
                                                  clusters)
    }
    if (subtypes) {
      stages[[t]][["forestryPassThrough"]] <- split[["passThrough"]]
    }
    if (!is.null(snvStage)) {
      stages[[t]][["snvRelocation"]] <- snvStage
    }
    if (!is.null(peatStage)) {
      stages[[t]][["peatRewetting"]] <- peatStage
    }
    stages[[t]][["optimisation"]] <- optimisation
  }

  # one object with the names "stage.from.to", in the order of the stages, and within a stage in the order
  # of the transitions of the optimisation
  items <- character(0)
  for (t in years) {
    for (stage in names(stages[[t]])) {
      if (ncol(stages[[t]][[stage]]) > 0) {
        items <- union(items, paste(stage, colnames(stages[[t]][[stage]]), sep = "."))
      }
    }
  }
  stageOrder <- c("disturbance", "recoveryForestry", "recovery", "maturation", "forestryPassThrough",
                  "snvRelocation", "peatRewetting", "optimisation")
  pairOrder <- colnames(stages[[1]][["optimisation"]])
  items <- items[order(match(sub("[.].*$", "", items), stageOrder), match(sub("^[^.]*[.]", "", items), pairOrder))]
  x <- array(0, dim = c(length(clusters), length(years), length(items)), dimnames = list(clusters, years, items))
  for (t in years) {
    for (stage in names(stages[[t]])) {
      if (ncol(stages[[t]][[stage]]) > 0) {
        x[, t, paste(stage, colnames(stages[[t]][[stage]]), sep = ".")] <- stages[[t]][[stage]]
      }
    }
  }
  x <- as.magpie(x, spatial = 1, temporal = 2)
  getSets(x, fulldim = FALSE)[2:3] <- c("t", "stage.from.to")
  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE)
  out(x, file)
}
