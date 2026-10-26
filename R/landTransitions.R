#' @title landTransitions
#' @description Land transitions of each time step by stage, in the order in which the model applies
#' them. Stages: disturbance (primary to secondary forest), recoveryForestry and recovery (othernat to
#' youngsecdf, from the forestry abandonment of the previous time step first), maturation (youngsecdf to
#' secondary forest), forestryPassThrough (area that enters forestry before it leaves), snvRelocation
#' (\code{p29_snv_relocation}, or \code{p30_snv_relocation} in older runs, if the run has it), peatRewetting
#' (reduction of managed land on drained peat, peatland realisation v2) and optimisation. Without sub-types,
#' the stages within a land type (recovery, forestryPassThrough) are left out, and only
#' \code{ov_lu_transitions} is required; stages whose symbols the run lacks are left out.
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

  # One item without its name, for arithmetic with objects of other items.
  .item <- function(x, item) {
    return(collapseDim(x[, , item], dim = 3))
  }

  # Object with the given item names and data dimension sets.
  .named <- function(x, names, sets) {
    getNames(x) <- names
    getSets(x, fulldim = FALSE)[3] <- sets
    return(x)
  }

  # Values of the previous time step; the first time step keeps its own values.
  .previous <- function(x) {
    return(setYears(x[, c(1, seq_len(nyears(x) - 1)), ], getYears(x)))
  }

  # Denominator of a share: values that are not positive become 1.
  .denominator <- function(x) {
    x[x <= 0] <- 1
    return(x)
  }

  # Transitions between sub-types ("from.to") fitted by iterative proportional fitting to the block totals
  # (mainFrom.mainTo: the transitions between main types and within a main type) and to the outflow (from)
  # and inflow (to) of each sub-type; noExternal marks the sub-types without outflow to other main types.
  .fitSubtypes <- function(pairs, blockTotal, outflow, inflow, noExternal) {
    from <- pairs[["from"]]
    to <- pairs[["to"]]
    blocks <- paste(mainOf[from], mainOf[to], sep = ".")
    y <- new.magpie(getCells(blockTotal), getYears(blockTotal), paste(blocks, from, to, sep = "."), fill = 1,
                    sets = c(getSets(blockTotal, fulldim = FALSE)[1:2], "mainFrom.mainTo.from.to"))

    # start values: 1 where the block has a positive total and the source type has outflow to other main types
    y <- y * (blockTotal[, , unique(blocks)] > 0)
    for (s in getNames(noExternal)) {
      items <- getNames(y)[from == s & mainOf[to] != mainOf[s]]
      y[, , items] <- y[, , items] * (1 - .item(noExternal, s))
    }

    .scale <- function(target, sum) {
      scale <- target / sum
      scale[sum <= 0] <- 0
      return(scale)
    }
    rowDims <- c("mainFrom", "mainTo", "to")
    colDims <- c("mainFrom", "mainTo", "from")
    # outflow of sub-types without allowed transitions cannot be placed
    unplaced <- 0
    if (!all(getNames(outflow) %in% from)) {
      unplaced <- max(abs(outflow[, , setdiff(getNames(outflow), from)]))
    }

    deviation <- Inf
    for (iter in seq_len(1000)) {
      blockSum <- dimSums(y, dim = c("from", "to"))
      y <- y * .scale(blockTotal[, , getNames(blockSum)], blockSum)
      rowSum <- dimSums(y, dim = rowDims)
      y <- y * .scale(outflow[, , getNames(rowSum)], rowSum)
      colSum <- dimSums(y, dim = colDims)
      y <- y * .scale(inflow[, , getNames(colSum)], colSum)

      previousDeviation <- deviation
      rowSum <- dimSums(y, dim = rowDims)
      deviation <- max(unplaced, abs(rowSum - outflow[, , getNames(rowSum)]))
      if (deviation < 1e-7 || previousDeviation - deviation < 1e-12) {
        break
      }
    }
    blockSum <- dimSums(y, dim = c("from", "to"))
    blockDeviation <- abs(blockTotal)
    blockDeviation[, , getNames(blockSum)] <- abs(blockSum - blockTotal[, , getNames(blockSum)])
    deviation <- max(deviation, blockDeviation)
    if (deviation >= 1e-6) {
      warning("sub-type split: margins deviate by ", signif(deviation, 3))
    }
    return(collapseDim(y, dim = c("mainFrom", "mainTo")))
  }

  # ---- symbols of the run ----------------------------------------------------------------------------

  transitions <- readGDX(gdx, "ov_lu_transitions", select = list(type = "level"), react = "silent")
  if (is.null(transitions)) {
    warning("Land transitions cannot be calculated as ov_lu_transitions could not be found in GDX file! ",
            "NULL is returned!")
    return(NULL)
  }
  years <- getYears(transitions)
  disturbance <- readGDX(gdx, "p35_disturbance_loss_primf", react = "silent")
  maturation <- readGDX(gdx, "p35_maturesecdf", react = "silent")
  # module 29 (cropland), or module 30 in runs before the cropland module
  snvRelocation <- readGDX(gdx, "p29_snv_relocation", "p30_snv_relocation", react = "silent", format = "first_found")
  if (!is.null(disturbance)) {
    disturbance <- dimSums(disturbance[, years, ], dim = 3)
  }
  if (!is.null(maturation)) {
    maturation <- dimSums(maturation[, years, ], dim = 3)
  }

  mainTypes <- c("crop", "past", "forestry", "primforest", "secdforest", "urban", "other")
  mainOf <- stats::setNames(mainTypes, mainTypes)

  # transitions between different main types
  getSets(transitions, fulldim = FALSE)[3] <- "from.to"
  between <- getItems(transitions, dim = 3.1, full = TRUE) != getItems(transitions, dim = 3.2, full = TRUE)
  optimisation <- transitions[, , between]

  # sub-types: the forestry types of module 32 and the other land types of module 35, their flows, and the
  # transitions between sub-types of the optimisation
  if (subtypes) {
    forestry <- .lvl("ov_land_forestry")[, years, ]
    forestryTypes <- getItems(forestry, dim = 3)
    otherTypes <- getItems(.lvl("ov_land_other"), dim = "othertype35")
    mainOf <- c(crop = "crop", past = "past",
                stats::setNames(rep("forestry", length(forestryTypes)), forestryTypes),
                primforest = "primforest", secdforest = "secdforest", urban = "urban",
                stats::setNames(rep("other", length(otherTypes)), otherTypes))
    types <- names(mainOf)

    # transitions the model allows: those between main types with a positive upper bound
    # (modules/10_land/landmatrix_dec18/presolve.gms); youngsecdf receives area only in the presolve. A forestry
    # type can expand and reduce in the same cluster and year; the model nets the transitions through
    # forestry, so the fit needs transitions from a forestry type to itself.
    upper <- readGDX(gdx, "ov_lu_transitions", select = list(type = "upper"))
    mainAllowed <- getNames(upper)[as.vector(dimSums((upper > 0) * 1, dim = c(1, 2)) > 0)]
    pairs <- expand.grid(from = types, to = types, stringsAsFactors = FALSE)
    fit <- (pairs[["from"]] != pairs[["to"]] & pairs[["to"]] != "youngsecdf" &
              paste(mainOf[pairs[["from"]]], mainOf[pairs[["to"]]], sep = ".") %in% mainAllowed) |
      (pairs[["from"]] == pairs[["to"]] & pairs[["from"]] %in% forestryTypes)
    pairs <- pairs[fit, ]

    # forestry types: expansion and reduction, plus the area that passes through forestry where the
    # transitions into forestry exceed the expansion, split over the types in proportion to their area of
    # the previous time step (in equal parts where the cluster had no forestry)
    mainOutflow <- dimSums(optimisation, dim = "to")
    mainInflow <- dimSums(optimisation, dim = "from")
    expansion <- .lvl("ov_landexpansion_forestry")[, years, forestryTypes]
    reduction <- .lvl("ov_landreduction_forestry")[, years, forestryTypes]
    # in runs before ov32_land_replant, expansion and reduction include the replanting of the harvested types
    # (harvest = reduction, re-established in the same time step): the area a type loses and regains
    if (is.null(readGDX(gdx, "ov32_land_replant", react = "silent"))) {
      harvested <- intersect(forestryTypes, c("plant", readGDX(gdx, "harvest32", react = "silent")))
      replant <- pmin(expansion[, , harvested], reduction[, , harvested])
      expansion[, , harvested] <- expansion[, , harvested] - replant
      reduction[, , harvested] <- reduction[, , harvested] - replant
    }
    reshuffle <- dimSums(expansion, dim = 3) - .item(mainInflow, "forestry")
    passThrough <- pmax(-reshuffle, 0)
    forestryArea <- .previous(forestry[, , forestryTypes])
    forestrySum <- dimSums(forestryArea, dim = 3)
    noForestry <- forestrySum == 0
    areaShare <- forestryArea / .denominator(forestrySum) * (1 - noForestry) + noForestry / length(forestryTypes)
    expansion <- expansion + passThrough * areaShare
    reduction <- reduction + passThrough * areaShare

    # other land types: reduction and harvest; harvested youngsecdf regrows as othernat
    otherReduction <- dimSums(.lvl("ov35_other_reduction")[, years, ], dim = "ac")
    otherHarvest <- dimSums(.lvl("ov35_hvarea_other")[, years, ], dim = "ac")

    # outflow and inflow of each sub-type
    mains <- setdiff(types, c(forestryTypes, otherTypes))
    outflow <- mbind(.named(mainOutflow[, , mains], mains, "from"), .named(reduction, forestryTypes, "from"),
                     .named(.item(otherReduction, "othernat") - .item(otherHarvest, "othernat"), "othernat", "from"),
                     .named(.item(otherReduction, "youngsecdf"), "youngsecdf", "from"))
    outflow <- pmax(outflow[, , types], 0)
    inflow <- mbind(.named(mainInflow[, , mains], mains, "to"), .named(expansion, forestryTypes, "to"),
                    .named(.item(mainInflow, "other") + .item(otherHarvest, "youngsecdf"), "othernat", "to"),
                    .named(.item(otherHarvest, "youngsecdf") * 0, "youngsecdf", "to"))
    inflow <- inflow[, , types]

    # block totals: the transitions between main types, and within a main type the changes of forestry
    # type and the harvest of youngsecdf
    blockTotal <- transitions
    getSets(blockTotal, fulldim = FALSE)[3] <- "mainFrom.mainTo"
    blockTotal[, , paste(mainTypes, mainTypes, sep = ".")] <- 0
    blockTotal[, , "forestry.forestry"] <- pmax(reshuffle, 0)
    blockTotal[, , "other.other"] <- .item(otherHarvest, "youngsecdf")

    # other land types whose whole reduction is harvest (regrowing as othernat)
    noExternal <- (otherReduction - otherHarvest) < 1e-9
    optimisation <- .fitSubtypes(pairs, blockTotal, outflow, inflow, noExternal)

    # the share of the inflow to the forestry types that passes through forestry
    from <- getItems(optimisation, dim = 3.1, full = TRUE)
    to <- getItems(optimisation, dim = 3.2, full = TRUE)
    intoForestry <- getNames(optimisation)[to %in% forestryTypes & !(from %in% forestryTypes)]
    passShare <- passThrough * areaShare / .denominator(expansion)
    getSets(passShare, fulldim = FALSE)[3] <- "to"
    passIn <- optimisation[, , intoForestry] * passShare
    optimisation[, , intoForestry] <- optimisation[, , intoForestry] - passIn

    # recovery from the forestry abandonment of the previous time step first (0 in the first time step)
    recovery <- dimSums(readGDX(gdx, "p35_forest_recovery_area")[, years, ], dim = 3)
    abandoned <- .previous(.item(transitions, "forestry.other"))
    abandoned[, 1, ] <- 0
    recoveryForestry <- pmin(recovery, abandoned)
  }
  pairOrder <- getNames(optimisation)

  # SNV relocation (q29_land_snv_trans), taken from the transitions from cropland to secondary forest and
  # other land in proportion to them
  snvStage <- NULL
  if (!is.null(snvRelocation)) {
    toSnv <- c("crop.secdforest", if (subtypes) "crop.othernat" else "crop.other")
    available <- dimSums(optimisation[, , toSnv], dim = 3)
    amount <- pmin(snvRelocation[, years, ], available)
    snvStage <- optimisation[, , toSnv] * (amount / .denominator(available))
    optimisation[, , toSnv] <- pmax(optimisation[, , toSnv] - snvStage, 0)
  }

  # land reduction on drained peat (q58_peatlandMan), with peatland realisation v2 (ov58_peatland by land58):
  # the decline of drained peat of each managed land type (0 in the first time step), taken from the
  # transitions out of that type in proportion to them
  peatStage <- NULL
  peatlandLevel <- readGDX(gdx, "ov58_peatland", select = list(type = "level"), react = "silent")
  if (!is.null(peatlandLevel) && getSets(peatlandLevel, fulldim = FALSE)[3] == "land58") {
    peatland <- PeatlandArea(gdx, level = "cell", sum = FALSE)
    drained <- peatland[, years, intersect(getNames(peatland), c("crop", "past", "forestry"))]
    peatLoss <- pmax(.previous(drained) - drained, 0)
    fromMain <- mainOf[getItems(optimisation, dim = 3.1, full = TRUE)]
    toMain <- mainOf[getItems(optimisation, dim = 3.2, full = TRUE)]
    peatStage <- list()
    for (type in getNames(peatLoss)) {
      items <- getNames(optimisation)[fromMain == type & toMain != type]
      if (length(items) > 0) {
        available <- dimSums(optimisation[, , items], dim = 3)
        share <- pmin(available, .item(peatLoss, type)) / .denominator(available)
        peatStage[[type]] <- optimisation[, , items] * share
      }
    }
    peatStage <- mbind(peatStage)
    optimisation[, , getNames(peatStage)] <- optimisation[, , getNames(peatStage)] - peatStage
    optimisation <- pmax(optimisation, 0)
    reduced <- as.vector(dimSums(peatStage, dim = c(1, 2))) > 0
    if (any(reduced)) {
      peatStage <- peatStage[, , reduced]
    } else {
      peatStage <- NULL
    }
  }

  # ---- stages of each time step, in the order in which the model applies them -------------------------

  stages <- list()
  if (!is.null(disturbance)) {
    stages[["disturbance"]] <- .named(disturbance, "primforest.secdforest", "from.to")
  }
  if (subtypes) {
    stages[["recoveryForestry"]] <- .named(recoveryForestry, "othernat.youngsecdf", "from.to")
    stages[["recovery"]] <- .named(recovery - recoveryForestry, "othernat.youngsecdf", "from.to")
  }
  if (!is.null(maturation)) {
    stages[["maturation"]] <- .named(maturation, if (subtypes) "youngsecdf.secdforest" else "other.secdforest",
                                     "from.to")
  }
  if (subtypes) {
    stages[["forestryPassThrough"]] <- passIn
  }
  stages[["snvRelocation"]] <- snvStage
  stages[["peatRewetting"]] <- peatStage
  stages[["optimisation"]] <- optimisation
  for (stage in names(stages)) {
    stages[[stage]] <- add_dimension(stages[[stage]], dim = 3.1, add = "stage", nm = stage)
  }

  # one object with the names "stage.from.to", within a stage in the order of the transitions of the
  # optimisation
  x <- mbind(stages)
  items <- getNames(x)
  x <- x[, , items[order(match(getItems(x, dim = 3.1, full = TRUE), names(stages)),
                         match(sub("^[^.]*[.]", "", items), pairOrder))]]
  getSets(x, fulldim = FALSE)[2:3] <- c("t", "stage.from.to")
  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE)
  out(x, file)
}
