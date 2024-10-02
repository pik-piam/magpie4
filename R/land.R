#' @title land
#' @description reads land out of a MAgPIE gdx file
#'
#' @importFrom magclass mbind read.magpie
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' or any other aggregation level defined in gdxAggregate
#' @param types NULL or a vector of strings. If NULL, all land types are used. Options are "crop", "past",
#' "forestry", "primforest","secdforest, "urban", "other", "primother" and "secdother"
#' @param subcategories NULL or vector of strings. If NULL, no subcategories are returned. Meaningful options
#'  are "crop, "forestry" and "other"
#' @param sum determines whether output should be land-type-specific (FALSE) or aggregated over all types (TRUE).
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @return land as MAgPIE object (Mha)
#' @author Jan Philipp Dietrich, Florian Humpenoeder, Benjamin Leon Bodirsky, Patrick v. Jeetze
#' @seealso \code{\link{reportLandUse}}
#' @examples
#' \dontrun{
#' x <- land(gdx)
#' }
#'
#' @importFrom magclass setCells

land <- function(gdx, file = NULL, level = "reg", types = NULL, subcategories = NULL,
                 sum = FALSE, dir = ".", spamfiledirectory = "") {

  dir <- getDirectory(dir, spamfiledirectory)

  if (level %in% c("grid","iso")) {
    x <- read.magpie(file.path(dir, "cell.land_0.5.mz"))
    if (length(getCells(x)) == "59199") {
      mapfile <- system.file("extdata", "mapping_grid_iso.rds", package="magpie4")
      map_grid_iso <- readRDS(mapfile)
      x <- setCells(x, map_grid_iso$grid)
    }
    x <- x[, "y1985", , invert = TRUE] # 1985 is currently the year before simulation start. has to be updated later
    x <- add_dimension(x, dim = 3.2, add = "sub", "total")
    if(level == "iso") x <- gdxAggregate(gdx, x , to = "iso", dir = dir)
    if (!is.null(subcategories)) {
      warning("argument subcategories is ignored for cellular data")
    }
  } else {
    x <- readGDX(gdx, "ov_land", "ovm_land", format = "first_found", select = list(type = "level"))
    #x <- add_dimension(x, dim = 3.2, add = "sub", "total")


    if (!is.null(subcategories)) {
      if ("crop" %in% subcategories) {
        croparea_land <- readGDX(gdx, "ov_area", select = list(type = "level"))
        fallow_land <- readGDX(gdx, "ov_fallow", select = list(type = "level"), react = "silent")
        if (is.null(fallow_land)) fallow_land <- new.magpie(getCells(croparea_land), getYears(croparea_land), fill = 0, sets = c("j.region","t","d3"))
        croptree_land <- readGDX(gdx, "ov_treecover", select = list(type = "level"), react = "silent")
        if (is.null(croptree_land)) croptree_land <- new.magpie(getCells(croparea_land), getYears(croparea_land), fill = 0, sets = c("j.region","t","d3"))
        crop <- mbind(add_dimension(dimSums(croparea_land, dim = 3), dim = 3.1, add = "land", "area"),
                      add_dimension(fallow_land, dim = 3.1, add = "land", "fallow"),
                      add_dimension(croptree_land, dim = 3.1, add = "land", "treecover"))
        getNames(crop,dim=1) <- paste("crop",getNames(crop,dim=1),sep="_")

        #crop <- add_dimension(crop, dim = 3.1, add = "land", "crop")
        # crop <- croparea(gdx, product_aggr = FALSE, level = "cell")
        # cropNoBio <- dimSums(crop[, , c("begr", "betr"), invert = TRUE])
        # cropBio <- dimSums(crop[, , c("begr", "betr")])
        # crop <- mbind(setNames(cropNoBio, "crop.nobio"), setNames(cropBio, "crop.bio"))
        # names(dimnames(crop)) <- names(dimnames(x))
        if (abs(sum(x[, , "crop"] - dimSums(crop, dim = 3))) > 2e-05) {
          warning("Cropland: Total and sum of subcategory land types diverge! Check your GAMS code!")
        }
      } else {
        crop <- x[, , "crop"]
      }
      if ("past" %in% subcategories) {
        warning("There are no subcatgories for pasture. Returning total pasture area")
        past <- x[, , "past"]
      } else {
        past <- x[, , "past"]
      }
      if ("forestry" %in% subcategories) {
        forestry <- readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"), react = "silent")
        if (suppressWarnings(!is.null(readGDX(gdx, "fcostsALL")) |
                             names(dimnames(forestry))[[3]] == "type32.ac")) {
          forestry <- dimSums(forestry, dim = "ac")
          getNames(forestry,dim=1) <- paste("forestry",getNames(forestry,dim=1),sep="_")
          names(dimnames(forestry)) <- names(dimnames(x))
        }
        if (abs(sum(x[, , "forestry"] - dimSums(forestry, dim = 3))) > 2e-05) {
          warning("Forestry: Total and sum of subcategory land types diverge! Check your GAMS code!")
        }
      } else {
        forestry <- x[, , "forestry"]
      }
      if ("primforest" %in% subcategories) {
        warning("There are no subcatgories for primforest Returning total primforest area")
        primforest <- x[, , "primforest"]
      } else {
        primforest <- x[, , "primforest"]
      }
      if ("secdforest" %in% subcategories) {
        warning("There are no subcatgories for secdforest. Returning total secdforest area")
        secdforest <- x[, , "secdforest"]
      } else {
        secdforest <- x[, , "secdforest"]
      }
      if ("urban" %in% subcategories) {
        warning("There are no subcatgories for urban land. Returning total urban area")
        urban <- x[, , "urban"]
      } else {
        urban <- x[, , "urban"]
      }
      if ("other" %in% subcategories) {
        other <- readGDX(gdx, "ov_land_other", "ov35_other", "ov_natveg_other",
                         select = list(type = "level"), react = "silent")
        other <- dimSums(other, dim = "ac")
        if(getSets(other)["d3.1"] == "othertype35") {
          getNames(other,dim=1) <- paste("other",getNames(other,dim=1),sep="_")
        } else {
          othernat <- other
          getNames(othernat,dim=1) <- "other_othernat"
          getSets(other)["d3.1"] <- "land"
          youngsecdf <- new.magpie(getCells(othernat), getYears(othernat), "other_youngsecdf", fill = 0, sets = c("j.region","t","land"))
          other <- mbind(othernat,youngsecdf)
        }
        names(dimnames(other)) <- names(dimnames(x))
        if (abs(sum(x[, , "other"] - dimSums(other, dim = 3))) > 2e-05) {
          warning("Other: Total and sum of subcategory land types diverge! Check your GAMS code!")
        }
      } else {
        other <- x[, , "other"]
      }
      x <- mbind(crop, past, forestry, primforest, secdforest, urban, other)
    }
  }
  if (is.null(x)) {
    warning("Land area cannot be calculated as land data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE, dir = dir)

  if (!is.null(types)) {
    if (any(grepl("primother", types)) | any(grepl("secdother", types))) {
      primsecdother <- PrimSecdOtherLand(x = "./cell.land_0.5.mz", ini_file = "./avl_land_full_t_0.5.mz", level = level)
      primsecdother <- add_dimension(primsecdother, dim = 3.2, add = "sub", "total")
      x <- mbind(x, primsecdother)
    }
    x <- x[, , types]
  }

  if (sum) {
    x <- dimSums(x, dim = 3)
  } else {
    x <- collapseNames(x)
  }
  if (all(is.null(getNames(x)))) {
    getNames(x) <- paste0(types, collapse = " ")
  } ## for netcdf files
  out(x, file)
}
