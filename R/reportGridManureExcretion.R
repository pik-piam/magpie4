#' @title reportGridManureExcretion
#' @description reports Manure with reprting names on grid level.
#'
#' @export
#'
#' @param gdx GDX file
#'
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' x <- reportGridManureExcretion(gdx)
#' }
#'
#' @section Grid-level manure excretion:
#' This function produces grid-level (0.5 degree) manure excretion and management data.
#' Includes total manure, breakdown by AWMS and livestock type, and losses from confinement.
#' @md

#'
reportGridManureExcretion <- function(gdx) {

  manure <- ManureExcretion(gdx, level = "grid")
  awms <- dimSums(manure, dim = "kli")
  kli <- dimSums(manure, dim = "awms")
  confinement <- collapseNames(manure[, , "confinement"])

  vm_manure_confinement <- collapseNames(readGDX(gdx, "ov_manure_confinement")[, , "level"][, , "nr"])
  vm_manure_confinement <- gdxAggregate(gdx = gdx, x = vm_manure_confinement, weight = manure[, , "confinement"], to = "grid", absolute = TRUE)

  pollutants <- c("n2o_n_direct", "nh3_n", "no2_n", "no3_n")
  f55_awms_recycling_share <- readGDX(gdx, "f55_awms_recycling_share")
  f51_ef3_confinement <- readGDX(gdx, "f51_ef3_confinement")
  im_maccs_mitigation <- readGDX(gdx, "im_maccs_mitigation")[, , "awms"][, , pollutants]
  f51_ef3_confinement <- f51_ef3_confinement * collapseNames(1 - im_maccs_mitigation)
  destiny <- add_columns(f51_ef3_confinement, addnm = c("recycling"), dim = 3.3)
  destiny[, , "recycling"] <- f55_awms_recycling_share
  # destiny[,,"n2_n"]<- (1-dimSums(destiny,dim=3.3,na.rm=TRUE))
  if (any(dimSums(destiny, dim = 3.3, na.rm = TRUE) > 1)) {
stop("error in emission factors")
}
  destiny <- gdxAggregate(gdx = gdx, x = destiny, to = "grid", absolute = FALSE)

  # memory problems
  emis1 <- vm_manure_confinement[, , c("livst_rum", "livst_milk")] * destiny[, , c("livst_rum", "livst_milk")]
  emis2 <- vm_manure_confinement[, , c("livst_pig", "livst_chick", "livst_egg")] * destiny[, , c("livst_pig", "livst_chick", "livst_egg")]
  emis1 <- dimSums(emis1, dim = "awms_conf")
  emis2 <- dimSums(emis2, dim = "awms_conf")
  destiny <- mbind(emis1, emis2)

  total <- setNames(dimSums(manure), "Manure")
  getNames(awms) <- paste0("Manure|+|", reportingnames(getNames(awms)))
  getNames(kli) <- paste0("Manure|++|", reportingnames(getNames(kli)))

  losses <- dimSums(destiny[, , pollutants], dim = "kli")
  getNames(losses) <- paste0("Manure|Manure In Confinements|Losses|", reportingnames(getNames(losses)))
  recycling <- dimSums(destiny[, , "recycling"], dim = "kli")
  getNames(recycling) <- paste0("Manure|Manure In Confinements|+|Recycled")
  confinement_loss <- dimSums(confinement, dim = 3) - recycling
  getNames(confinement_loss) <- paste0("Manure|Manure In Confinements|+|Losses")

  out <- mbind(total, awms, kli, recycling, confinement_loss, losses)

  return(out)
}
