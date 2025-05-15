dissagLandConsv <- function(gdx, cfg, map_file, wdpa_hr_file, consv_prio_hr_file) {
  land_consv_lr <- gdx2::readGDX(gdx, "pm_land_conservation", react = "silent")
  land_consv_lr <- dimSums(land_consv_lr, dim = 3.2)
  wdpa_hr <- read.magpie(wdpa_hr_file)
  map <- readRDS(map_file)

  # create full time series
  land_consv_hr <- new.magpie(map[, "cell"], getYears(land_consv_lr), getItems(wdpa_hr, dim = 3.2),
    fill = 0, sets = c("x.y.iso", "year", "data")
  )

  iso <- gdx2::readGDX(gdx, "iso")
  consv_iso <- gdx2::readGDX(gdx, "policy_countries22")
  consv_iso <- consv_iso[consv_iso %in% getItems(wdpa_hr, dim = 1.3)]
  if (length(consv_iso) == 0) {
    warning("No countries selected in land conservation disaggregation. Results may be erroneous")
  }

  base_protect_select <- cfg$gms$c22_base_protect
  base_protect_noselect <- cfg$gms$c22_base_protect_noselect

  if (!all(c(base_protect_select, base_protect_noselect) %in% "none")) {

    if (base_protect_noselect != "none") {
      land_consv_hr[, getYears(land_consv_hr), ] <- collapseDim(wdpa_hr[, nyears(wdpa_hr), base_protect_noselect], dim = 3.1)
      land_consv_hr[, getYears(wdpa_hr), ] <- collapseDim(wdpa_hr[, , base_protect_noselect], dim = 3.1)
    }
    if (base_protect_select != "none") {
      land_consv_hr[consv_iso, , ] <- collapseDim(wdpa_hr[consv_iso, nyears(wdpa_hr), base_protect_select], dim = 3.1)
    } else {
      land_consv_hr[consv_iso, , ] <- 0
    }
  }

  consv_select <- cfg$gms$c22_protect_scenario
  consv_noselect <- cfg$gms$c22_protect_scenario_noselect

  if (!all(c(consv_select, consv_noselect) %in% "none")) {
    if (file.exists(consv_prio_hr_file)) {
      consv_prio_all <- read.magpie(consv_prio_hr_file)
      consv_prio_hr <- new.magpie(
        cells_and_regions = map[, "cell"],
        names = getNames(consv_prio_all, dim = 2), fill = 0,
        sets = c("x.y.iso", "year", "data")
      )

      if (consv_noselect != "none") {
        consv_prio_hr <- collapseDim(consv_prio_all[, , consv_noselect], dim = 3.1)
      }
      if (consv_select != "none") {
        consv_prio_hr[consv_iso, , ] <- collapseDim(consv_prio_all[consv_iso, , consv_select], dim = 3.1)
      } else {
        consv_prio_hr[consv_iso, , ] <- 0
      }
      # future conservation only pertains to natveg
      consv_prio_hr[, , c("crop", "past", "forestry", "urban")] <- 0
      consv_fader <- gdx2::readGDX(gdx, "p22_conservation_fader", format = "first_found")
      consv_prio_hr <- consv_prio_hr * consv_fader[, getYears(land_consv_hr), ]

      # add conservation priority areas
      land_consv_hr <- (land_consv_hr + consv_prio_hr)
    } else {
      warning(paste(
        "Future land conservation used in MAgPIE run but high resolution",
        "conservation priority data for disaggregation not found."
      ))
    }
  }
  # Due to internal constraints and compensation (e.g. NDC forest conservation)
  # the actual land conservation can sometimes be smaller than the land
  # conservation in the input data (this can especially happen also if
  # land restoration is switched off). Therefore a scaling is applied here separately
  # for grassland and natural vegetation
  natveg <- c("primforest", "secdforest", "other")
  consv_sum_lr <- mbind(
    land_consv_lr[, , "past"],
    setNames(dimSums(land_consv_lr[, , natveg], dim = 3), "natveg")
  )
  consv_sum_hr_agg <- mbind(
    toolAggregate(land_consv_hr[, , "past"], map, from = "cell", to = "cluster"),
    toolAggregate(setNames(dimSums(land_consv_hr[, , natveg], dim = 3), "natveg"),
      map,
      from = "cell", to = "cluster"
    )
  )
  consv_scaling <- consv_sum_lr / consv_sum_hr_agg
  consv_scaling[is.na(consv_scaling) | is.infinite(consv_scaling)] <- 1
  consv_scaling <- toolAggregate(consv_scaling, map, from = "cluster", to = "cell")
  land_consv_hr[, , "past"] <- consv_scaling[, , "past"] * land_consv_hr[, , "past"]
  land_consv_hr[, , natveg] <- consv_scaling[, , "natveg"] * land_consv_hr[, , natveg]
  return(land_consv_hr)
}
