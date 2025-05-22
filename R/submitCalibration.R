#' @title submitCalibration
#' @description Submits Calibration Factors of current run to calibration archive. Currently covers calibration factors for yields and land conversion costs.
#' This is useful to make runs more comparable to each other. The function can be also used as part of a script running
#' a collection of runs.
#' @param name name under which the calibration should be stored. Should be as
#' self-explaining as possible. The total file name has the format calibration_<name>_<date>.tgz.
#' @param file path to a f14_yld_calib.csv and f39_calib.cs3 (older version f39_calib.csv) file (in this order). Alternatively a fulldata.gdx file containing the calibration factors can be used. Supported file types are "csv", "cs3" and "gdx".
#' @param archive path to the archive the calibration factors should be stored
#' @return file name of the stored calibration factors (useful for scripts in which you might want to re-use a calibration
#' setting at a later stage again)
#' @importFrom tools file_ext
#' @importFrom gms tardir
#' @importFrom magclass read.magpie write.magpie
#' @author Jan Philipp Dietrich, Florian Humpenoeder, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' fname <- submitCalibration("TestCalibration", file = "fulldata.gdx")
#' }
#' @export

submitCalibration <- function(name, file = c("modules/14_yields/input/f14_yld_calib.csv", "modules/39_landconversion/input/f39_calib.cs3"), archive = "/p/projects/landuse/data/input/calibration") {
  if (!all(unique(file_ext(file)) %in% c("csv", "cs3", "gdx"))) {
    stop("Different file types are not supported!")
  } else {
    ftype <- unique(file_ext(file))
    if ("gdx" %in% ftype & length(ftype) == 1) {
      d <- readGDX(file, "f14_yld_calib", react = "silent")
      e <- readGDX(file, "f39_calib", react = "silent")
    } else if (any(c("csv", "cs3") %in% ftype) & length(ftype) <= 2) {
      if(file.exists(file[1])) {
        d <- read.magpie(file[1])
      } else {
        d <- NULL
        warning(paste("File", file[1], "not found!"))
      }
      if (!is.na(file[2]) & file.exists(file[2])) {
        e <- read.magpie(file[2])
      } else if (!is.na(file[2]) & file.exists(gsub("cs3", "csv", file[2]))) {
        e <- read.magpie(gsub("cs3", "csv", file[2]))
      } else {
        e <- NULL
        if (!is.na(file[2])) warning(paste("File", file[2], "not found!"))
      }
    } else {
      stop("Unsupported file type!")
    }
    fname <- format(Sys.time(), paste0("calibration_", name, "_%d%b%y.tgz"))
    i <- 1
    while (file.exists(paste0(archive, "/", fname))) {
      i <- i + 1
      fname <- format(Sys.time(), paste0("calibration_", name, "_%d%b%y_", i, ".tgz"))
    }
    tdir <- tempdir()
    unlink(paste0(tdir, "/*"))
    if(!is.null(d)) {
      write.magpie(d, paste0(tdir, "/f14_yld_calib.csv"))
    }
    if (!is.null(e)) {
      if (ndim(e, dim = 3) == 1) {
        write.magpie(e, paste0(tdir, "/f39_calib.csv"))
      } else {
        write.magpie(e, paste0(tdir, "/f39_calib.cs3"))
      }
    }
    tardir(tdir, tarfile = paste0(archive, "/", fname))
    return(fname)
  }
}
