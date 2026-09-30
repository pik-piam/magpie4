#' @title submitCalibration
#' @description Submits Calibration Factors of current run to calibration archive. Currently covers calibration factors for yields and land conversion costs.
#' This is useful to make runs more comparable to each other. The function can be also used as part of a script running
#' a collection of runs.
#' @param name name under which the calibration should be stored. Should be as
#' self-explaining as possible. The total file name has the format calibration_<name>_<date>.tgz.
#' @param file path to a f14_yld_calib.csv, f39_calib.cs3 (older version f39_calib.csv) and f39_calib_past.csv file (in this order; the third is optional). Alternatively a fulldata.gdx file containing the calibration factors can be used. Supported file types are "csv", "cs3" and "gdx".
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

submitCalibration <- function(name,
  file = c("modules/14_yields/input/f14_yld_calib.csv",
           "modules/39_landconversion/input/f39_calib.cs3",
           "modules/39_landconversion/input/f39_calib_past.csv"),
  archive = "/p/projects/landuse/data/input/calibration"
) {
  # first existing path wins; warns about the first candidate if none exist
  readFirstExisting <- function(paths) {
    paths <- paths[!is.na(paths)]
    if (length(paths) == 0) return(NULL)
    hit <- paths[file.exists(paths)]
    if (length(hit) == 0) {
      warning("File ", paths[1], " not found!")
      return(NULL)
    }
    read.magpie(hit[1])
  }

  ftype <- unique(file_ext(file))
  if (identical(ftype, "gdx")) {
    d <- readGDX(file, "f14_yld_calib", react = "silent")
    e <- readGDX(file, "f39_calib", react = "silent")
    p <- readGDX(file, "f39_calib_past", react = "silent")
  } else if (all(ftype %in% c("csv", "cs3"))) {
    d <- readFirstExisting(file[1])
    e <- readFirstExisting(c(file[2], sub("\\.cs3$", ".csv", file[2])))
    p <- readFirstExisting(file[3])
  } else {
    stop("Unsupported file type(s): ", paste(ftype, collapse = ", "))
  }

  base <- paste0("calibration_", name, "_", format(Sys.Date(), "%d%b%y"))
  fname <- paste0(base, ".tgz")
  i <- 1
  while (file.exists(file.path(archive, fname))) {
    i <- i + 1
    fname <- paste0(base, "_", i, ".tgz")
  }

  tdir <- tempfile("calibration")
  dir.create(tdir)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  if (!is.null(d)) {
    write.magpie(d, file.path(tdir, "f14_yld_calib.csv"))
  }
  if (!is.null(e)) {
    write.magpie(e, file.path(tdir, paste0("f39_calib.", if (ndim(e, dim = 3) == 1) "csv" else "cs3")))
  }
  if (!is.null(p)) {
    write.magpie(p, file.path(tdir, "f39_calib_past.csv"))
  }
  tardir(tdir, tarfile = file.path(archive, fname))
  return(fname)
}
