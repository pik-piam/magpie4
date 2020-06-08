#' @title submitCalibration
#' @description Submits Calibration Factors of current run to calibration archive. 
#' This is useful to make runs more comparable to each other. The function can be also used as part of a script running
#' a collection of runs.
#' @param name name under which the calibration should be stored. Should be as 
#' self-explaining as possible. The total file name has the format calibration_<name>_<date>.tgz.
#' @param file path to a f14_yld_calib.csv or fulldata.gdx file containing the yield calibration factors.
#' @param archive path to the archive the calibration factors should be stored
#' @return file name of the stored calibration factors (useful for scripts in which you might want to re-use a calibration
#' setting at a later stage again)
#' @importFrom tools file_ext
#' @importFrom gdx readGDX
#' @importFrom gms tardir
#' @importFrom magclass read.magpie write.magpie
#' @author Jan Philipp Dietrich
#' @examples
#'   \dontrun{
#'     fname <- submitCalibration("TestCalibration",file="fulldata.gdx")
#'   }
#' @export

submitCalibration <- function(name, file="modules/14_yields/input/f14_yld_calib.csv", archive="/p/projects/landuse/data/input/calibration") {
  ftype <- file_ext(file)
  if(ftype=="gdx") {
    d <- readGDX(file,"f14_yld_calib")
  } else if(ftype=="csv") {
    d <- read.magpie(file)
  } else {
    stop("Unsupported file type!")
  }
  fname <- format(file.mtime(file), paste0("calibration_",name,"_%d%b%y.tgz"))
  i <- 1
  while(file.exists(paste0(archive,"/",fname))) {
    i <- i + 1
    fname <- format(Sys.time(), paste0(name,"_%d%b%y_",i,".tgz"))
  }
  tdir <- tempdir()
  unlink(paste0(tdir,"/*"))
  write.magpie(d,paste0(tdir,"/f14_yld_calib.csv"))
  tardir(tdir, tarfile=paste0(archive,"/",fname))
  return(fname)
}
