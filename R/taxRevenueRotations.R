#' @title taxRevenueRotations
#' @description calculates taxes of crop rotations as difference between the selected scenario
#' and the baseline scenario that shall capture the internalized incentives for crop rotations.
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo
#' @param dir spamfiledirectory
#' @param penalty "OnlyTaxRevenue" provides the tax Revenues from a rotation tax/subsidy.
#' "OnlyInternalizedServices" provides the penalty by foregone Ecosystem Services,
#' the part of the externality which is internalized by the farmer independent of the tax.
#' "FullPenalty" provides the sum of both, which is what the model sees.
#'
#' @author Benjamin Leon Bodirsky

#' @examples
#' \dontrun{
#' x <- wageRent(gdx)
#' }
#'

taxRevenueRotations <- function(gdx, file = NULL, level = "regglo", dir = ".", penalty="onlyTaxRevenue") {

  vm_rotation_penalty <- collapseNames(readGDX(gdx, "ov_rotation_penalty")[,,"level"])

  if (suppressWarnings(is.null(readGDX(gdx, "i30_rotation_incentives")))) {
    cat("rotation module not activated, so tax Revenues set to zero")
    if(sum(vm_rotation_penalty)>0) {stop("rotation penalty should be zero if module is not activated")}
    tax=vm_rotation_penalty
  } else {

    # for testing

    i30_rotation_incentives <- readGDX(gdx, "i30_rotation_incentives")[,getYears(vm_rotation_penalty),]

    # for reproduction
    f30_rotation_incentives <- readGDX(gdx, "f30_rotation_incentives")
    i30_rotation_incentives <- readGDX(gdx, "i30_rotation_incentives")[,getYears(vm_rotation_penalty),]
    v30_penalty <- collapseNames(readGDX(gdx, "ov30_penalty")[,,"level"])
    v30_penalty_max_irrig <- collapseNames(readGDX(gdx,"ov30_penalty_max_irrig")[,,"level"])
    rotamax_red30 <- readGDX(gdx,"rotamax_red30")

    full_penalty= (
      dimSums(v30_penalty * i30_rotation_incentives,dim="rota30") +
        dimSums(v30_penalty_max_irrig * i30_rotation_incentives[,,rotamax_red30], dim="rotamax30")
    )

    internalized_services =
      (
        dimSums(v30_penalty * f30_rotation_incentives[,,"default"],dim="rota30") +
          dimSums(v30_penalty_max_irrig * f30_rotation_incentives[,,"default"][,,readGDX(gdx,"rotamax_red30")], dim="rotamax30")
      )

    tax = full_penalty-internalized_services

    test <- FALSE
    if (test==TRUE){
      testvalue1 = sum((vm_rotation_penalty))
      testvalue2 = sum((
        dimSums(v30_penalty * i30_rotation_incentives,dim="rota30") +
        dimSums(v30_penalty_max_irrig * i30_rotation_incentives[,,readGDX(gdx,"rotamax_red30")], dim="rotamax30")
      ))
      if (abs(testvalue1-testvalue2)>0.1) {stop("test not passed")}
      if (sum(tax[,"y2000"])!=0) {stop("Tax should be zero in 2000")}
    }

    if (penalty=="FullPenalty") {
      tax <- full_penalty
    } else if (penalty=="OnlyInternalizedService") {
      tax <- internalized_services
    }

  }
  out <- gdxAggregate(gdx = gdx,x = tax, to = level,
                      weight = "land", types = "crop", absolute = TRUE,
                      dir = dir)
  return(out)
}
