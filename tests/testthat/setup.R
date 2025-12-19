# Setup paths
gdxUrl <- "https://rse.pik-potsdam.de/data/example/magpie-fulldata/magpie-default-fulldata.gdx"
fixturesDir <- "tmp_fixtures"
gdxPath <- file.path(fixturesDir, "magpie-default-fulldata.gdx")

# Create fixtures directory if it doesn't exist
if (!dir.exists(fixturesDir)) {
  dir.create(fixturesDir, recursive = TRUE)
}

# Download GDX file if it doesn't exist or is older than 36 hours
shouldDownload <- FALSE
if (!file.exists(gdxPath)) {
  shouldDownload <- TRUE
} else {
  fileInfo <- file.info(gdxPath)
  fileAge <- difftime(Sys.time(), fileInfo$mtime, units = "hours")
  if (fileAge > 36) {
    shouldDownload <- TRUE
  }
}

if (shouldDownload) {
  withr::local_options(timeout = 10 * 60) # 10 Minutes timeout
  download.file(gdxUrl, gdxPath, mode = "wb", quiet = TRUE)
}