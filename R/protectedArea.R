#' @title protectedArea
#' @description reads protectedArea out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param cfg config file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "grid", "iso, "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum sum over land pools (default = FALSE)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @details protected areas in primforest, secdforest and other land
#' @return protected area in Mha
#' @author Florian Humpenoeder, Patrick v. Jeetze
#' @importFrom madrat toolAggregate
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums mbind getNames setNames getCells getYears new.magpie
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- protectedArea(gdx)
#' }
#'
protectedArea <- function(gdx, cfg = NULL, file = NULL, level = "cell", sum = FALSE, dir = "."){

  # read in protected areas
  if (level %in% c("grid","iso")) {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package="magpie4")
    map_grid_iso <- readRDS(mapfile)
    cmap <- Sys.glob(file.path(dir, "clustermap_*.rds"))

    wdpa_hr_file <- c(
      file.path(dir, "wdpa_baseline_0.5.mz"),
      "input/wdpa_baseline_0.5.mz",
      "modules/22_land_conservation/input/wdpa_baseline_0.5.mz",
      "../input/wdpa_baseline_0.5.mz",
      "../modules/22_land_conservation/input/wdpa_baseline_0.5.mz",
      "../../input/wdpa_baseline_0.5.mz",
      "../../modules/22_land_conservation/input/wdpa_baseline_0.5.mz.mz"
    )
    wdpa_hr_file <- suppressWarnings(wdpa_hr_file[min(which(file.exists(wdpa_hr_file)))])

    consv_prio_hr_file <- c(
      file.path(dir, "consv_prio_areas_0.5.mz"),
      "input/consv_prio_areas_0.5.mz",
      "modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
      "../input/consv_prio_areas_0.5.mz",
      "../modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
      "../../input/consv_prio_areas_0.5.mz",
      "../../modules/22_land_conservation/input/consv_prio_areas_0.5.mz"
    )
    consv_prio_hr_file <- suppressWarnings(consv_prio_hr_file[min(which(file.exists(consv_prio_hr_file)))])

    if (!is.na(wdpa_hr_file)) {

      land_consv_lr <- readGDX(gdx, "pm_land_conservation", react = "silent")
      wdpa_hr <- read.magpie(wdpa_hr_file)

      # create full time series
      land_consv_hr <- new.magpie(map_grid_iso$grid, getYears(land_consv_lr), getNames(wdpa_hr))
      land_consv_hr[, getYears(land_consv_hr), ] <- wdpa_hr[, nyears(wdpa_hr), ]
      land_consv_hr[, getYears(wdpa_hr), ] <- wdpa_hr

      if (!is.null(cfg)) {
        if (!all(c(cfg$gms$c22_protect_scenario, cfg$gms$c22_protect_scenario_noselect) %in% "none")) {
          if (!is.na(consv_prio_hr_file)) {
            consv_prio_all <- read.magpie(consv_prio_hr_file)
            consv_prio_hr <- new.magpie(
              cells_and_regions = map_grid_iso$grid,
              names = getNames(consv_prio_all, dim = 2), fill = 0
            )
            iso <- readGDX(gdx, "iso")
            consv_iso <- readGDX(gdx, "policy_countries22")
            consv_iso <- consv_iso[consv_iso %in% getItems(consv_prio_all, dim = 1.1)]
            consv_select <- cfg$gms$c22_protect_scenario
            consv_noselect <- cfg$gms$c22_protect_scenario_noselect

            if (consv_noselect != "none") {
              consv_prio_hr <- collapseDim(consv_prio_all[, , consv_noselect], dim = 3.1)
            }
            if (consv_select != "none") {
              consv_prio_hr[consv_iso, , ] <- collapseDim(consv_prio_all[consv_iso, , consv_select], dim = 3.1)
            } else if (consv_select == "none") {
              consv_prio_hr[consv_iso, , ] <- 0
            }
            # future conservation only pertains to natveg
            consv_prio_hr[, , c("crop", "past", "forestry", "urban")] <- 0

            consv_fader <- readGDX(gdx, "p22_conservation_fader", format = "first_found")
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
      }

      # Due to internal constraints and compensation (e.g. NDC forest conservation)
      # the actual land conservation can sometimes be smaller than the land
      # conservation in the input data (this can especially happen also if
      # land restoration is switched off). Therefore a scaling is applied here separately
      # for grassland and natural vegetation
      if (!is.null(land_consv_lr)) {
        natveg <- c("primforest", "secdforest", "other")
        consv_sum_lr <- mbind(
          dimSums(land_consv_lr[, , "past"], 3.2),
          setNames(dimSums(land_consv_lr[, , natveg], dim = 3), "natveg")
        )
        consv_sum_hr_agg <- mbind(
          toolAggregate(land_consv_hr[, , "past"], cmap, from = "cell", to = "cluster"),
          toolAggregate(setNames(dimSums(land_consv_hr[, , natveg], dim = 3), "natveg"),
            cmap,
            from = "cell", to = "cluster"
          )
        )
        consv_scaling <- consv_sum_lr / consv_sum_hr_agg
        consv_scaling[is.na(consv_scaling) | is.infinite(consv_scaling)] <- 1
        consv_scaling <- toolAggregate(consv_scaling, cmap, from = "cluster", to = "cell")
        land_consv_hr[, , "past"] <- consv_scaling[, , "past"] * land_consv_hr[, , "past"]
        land_consv_hr[, , natveg] <- consv_scaling[, , "natveg"] * land_consv_hr[, , natveg]
      }
      a <- land_consv_hr
    } else {
      stop("Cannot find gridded land conservation input data in output folder.")
    }
    if(level == "iso") {
      a <- gdxAggregate(gdx, a , to = "iso", dir = dir)
    }
  } else {
    a <- readGDX(gdx, "pm_land_conservation", react = "silent")
    # sum protection and restoration area
    if (!is.null(a)) {
      a <- dimSums(a, dim = 3.2)
    }

    if (is.null(a)) {
      a <- readGDX(gdx, "p35_save_natveg", react = "silent")
      if (!is.null(a)) {
        a <- mbind(a, new.magpie(getCells(a), getYears(a), c("crop", "past", "forestry", "urban"), fill = 0))
      }
    }

    if (is.null(a)) {
      primforest <- setNames(readGDX(gdx, "p35_save_primforest", react = "silent"), "primforest")
      secdforest <- setNames(readGDX(gdx, "p35_save_secdforest", react = "silent"), "secdforest")
      other <- setNames(readGDX(gdx, "p35_save_other", react = "silent"), "other")
      a <- mbind(
        primforest,
        secdforest,
        other
      )
      a <- mbind(a, new.magpie(getCells(a), getYears(a), c("crop", "past", "forestry", "urban"), fill = 0))
    }
    a <- gdxAggregate(gdx, a, to = level, absolute = TRUE, dir = dir)
  }

  names(dimnames(a))[1] <- "j"

  # sum
  if (sum) a <- dimSums(a, dim = 3.1)

  out(a, file)
}
