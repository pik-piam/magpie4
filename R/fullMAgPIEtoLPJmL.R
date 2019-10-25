#' @title fullMAgPIEtoLPJmL
#' @description writes gridded (disaggregated) magpie output into netcdf file
#' 
#' @export
#' 
#' @param gdx GDX file

#' @return netcdf (.nc) file
#' @author Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     fullMAgPIEtoLPJmL(gdx)
#'   }
#' 

fullMAgPIEtoLPJmL <- function(gdx)
{
  write.magpie(reportGridLand(gdx), file_type = "nc", file_name = "LandAreaPhysical.nc") # ?
  write.magpie(reportGridCroparea(gdx), file_type = "nc", file_name = "CroplandAreaPhysical.nc") # ?
  # calcOutput("ValidCellularYields", aggregate = FALSE, file = "YieldsPhysical.nc")
  # calcOutput("ValidCellularNitrogenBudgetCropland", aggregate = FALSE,
  #            file = "NitrogenBudgetCropland.nc")
  # calcOutput("ValidCellularNitrogenBudgetPasture", aggregate = FALSE,
  #            file = "NitrogenBudgetPasture.nc")
  # calcOutput("ValidCellularResidueDemand", aggregate = FALSE,
  #            file = "AbovegroundCropResidueDemand.nc")
  # calcOutput("ValidCellularCroplandNitrogenInputs", aggregate = FALSE,
  #            file = "NitrogenInputsByCropType.nc")
  # calcOutput("ValidCellularCroplandNitrogenWithdrawals", aggregate = FALSE,
  #            file = "NitrogenWithdrawalsByCropType.nc")
  # calcOutput("ValidCellularCroplandNitrogenWithdrawals", irrigation = "rainfed",
  #            aggregate = FALSE, file = "NitrogenWithdrawalsByCropTypeRainfed.nc")
  # calcOutput("ValidCellularCroplandNitrogenWithdrawals", irrigation = "irrigated",
  #            aggregate = FALSE, file = "NitrogenWithdrawalsByCropTypeIrrigated.nc")
  # calcOutput("ValidCellularCroplandNitrogenSurplus", aggregate = FALSE,
  #            file = "NitrogenSurplusByCropType.nc")
}