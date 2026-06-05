#' @title mappingToLongFormat
#' @description Derives a report aggregation mapping (sourceRegion to targetRegion)
#'   from a country-level region mapping. The input must have at least the columns
#'   \code{countryname}, \code{country}, and \code{region}. Any additional columns
#'   are treated as definitions of aggregated macro regions: each unique non-empty
#'   value defines a target region, and all source regions containing at least one
#'   country assigned to that value are mapped to it.
#'
#'   The output mapping has three blocks (each sorted by sourceRegion):
#'   \enumerate{
#'     \item each source region mapped to itself (regional resolution)
#'     \item each source region mapped to \code{"GLO"} (global aggregate)
#'     \item source regions mapped to every additional aggregated region they belong to
#'   }
#' @note This function is a workaround as in magpie4 we can not use the same mapping
#' formulas that we could use in madrat functions. If the magpie4 aggregation logic
#' is ever rewritten, this function may become obsolete.
#'
#' @param mappingOrFileName Either a data frame containing the country-level
#'   region mapping, or a character string giving the path to a CSV-file.
#' @return A data frame with columns \code{sourceRegion} and \code{targetRegion}.
#' @author Kristine Karstens, Patrick Rein
#' @family Spatial
#' @export
mappingToLongFormat <- function(mappingOrFileName) {
  if (is.character(mappingOrFileName)) {
    if (!file.exists(mappingOrFileName)) {
      stop("Input file not found: ", mappingOrFileName)
    }
    m <- read.csv(mappingOrFileName, sep = ";", stringsAsFactors = FALSE)
  } else if (is.data.frame(mappingOrFileName)) {
    m <- mappingOrFileName
  } else {
    stop("mappingOrFileName must be a file path (character) or a data.frame")
  }

  # Columns that define additional macro regions (everything beyond the
  # three standard columns countryname / country / region)
  standardCols  <- c("countryname", "country", "region")
  aggregateCols <- setdiff(names(m), standardCols)

  regions <- sort(unique(m$region))

  # Block 1: each region maps to itself
  selfBlock <- data.frame(sourceRegion = regions,
                          targetRegion = regions,
                          stringsAsFactors = FALSE)

  # Block 2: each region maps to the global aggregate
  gloBlock <- data.frame(sourceRegion = regions,
                         targetRegion = rep("GLO", length(regions)),
                         stringsAsFactors = FALSE)

  # Block 3: for every additional macro region column, find which source
  # regions contain countries assigned to each target value and build mappings
  aggBlocks <- lapply(aggregateCols, function(col) {
    values <- m[[col]]
    # unique non-empty target region names defined in this column
    macroRegions <- sort(unique(values[!is.na(values) & nchar(trimws(values)) > 0]))
    do.call(rbind, lapply(macroRegions, function(macroRegion) {
      src <- sort(unique(m$region[!is.na(values) & trimws(values) == macroRegion]))
      return(data.frame(sourceRegion = src,
                        targetRegion = rep(macroRegion, length(src)),
                        stringsAsFactors = FALSE))
    }))
  })

  out <- rbind(selfBlock, gloBlock, do.call(rbind, aggBlocks))
  return(out)
}
