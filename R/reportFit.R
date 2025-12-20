#' @title reportFit
#' @description reports fit and error indicators compared to initial values
#'
#' @export
#'
#' @param gdx GDX file
#' @param type type of indicator. Options: R2, MAE,  MPE (mean percentage error - bias), MAPE (mean absolute percentage error)
#' @param level level at which the regional and global bias should be reported. Options "cell" or "grid"
#' @return Selected error indicator
#' @author Edna Molina Bacca, Patrick v. Jeetze
#' @importFrom magpiesets reportingnames
#' @importFrom madrat toolAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- reportFit(gdx,type)
#'   }
#'
#'
#' @section Cluster-level fit indicator variables:
#' Name | Unit | Meta
#' ---|---|---
#' Fit\|Cluster\|Land Cover | MAPE/MAE/MPE | Fit indicator for overall land cover
#' Fit\|Cluster\|Land Cover\|Cropland | MAPE/MAE/MPE | Fit indicator for cropland
#' Fit\|Cluster\|Land Cover\|Pastures and Rangelands | MAPE/MAE/MPE | Fit indicator for pasture (when no range distinction)
#' Fit\|Cluster\|Land Cover\|Managed pastures | MAPE/MAE/MPE | Fit indicator for managed pastures (when range available)
#' Fit\|Cluster\|Land Cover\|Rangelands | MAPE/MAE/MPE | Fit indicator for rangelands (when range available)
#' Fit\|Cluster\|Land Cover\|Urban Area | MAPE/MAE/MPE | Fit indicator for urban area
#' Fit\|Cluster\|Land Cover\|Other Land | MAPE/MAE/MPE | Fit indicator for other land
#' Fit\|Cluster\|Land Cover\|Forest\|Natural Forest\|Primary Forest | MAPE/MAE/MPE | Fit indicator for primary forest
#' Fit\|Cluster\|Land Cover\|Forest\|Natural Forest\|Secondary Forest | MAPE/MAE/MPE | Fit indicator for secondary forest
#' Fit\|Cluster\|Land Cover\|Forest\|Planted Forest | MAPE/MAE/MPE | Fit indicator for planted forest
#' Fit\|Cluster\|Land Cover\|Cropland\|{crop} | MAPE/MAE/MPE | Fit indicator for individual crop types
#'
#' @section Grid-level fit indicator variables:
#' Name | Unit | Meta
#' ---|---|---
#' Fit\|Grid\|Land Cover | MAPE/MAE/MPE | Grid-level fit indicator for overall land cover
#' Fit\|Grid\|Land Cover\|Cropland | MAPE/MAE/MPE | Grid-level fit indicator for cropland
#' Fit\|Grid\|Land Cover\|Pastures and Rangelands | MAPE/MAE/MPE | Grid-level fit indicator for pasture
#' Fit\|Grid\|Land Cover\|Managed pastures | MAPE/MAE/MPE | Grid-level fit indicator for managed pastures (when range available)
#' Fit\|Grid\|Land Cover\|Rangelands | MAPE/MAE/MPE | Grid-level fit indicator for rangelands (when range available)
#' Fit\|Grid\|Land Cover\|Urban Area | MAPE/MAE/MPE | Grid-level fit indicator for urban area
#' Fit\|Grid\|Land Cover\|Other Land | MAPE/MAE/MPE | Grid-level fit indicator for other land
#' Fit\|Grid\|Land Cover\|Forest\|Natural Forest\|Primary Forest | MAPE/MAE/MPE | Grid-level fit indicator for primary forest
#' Fit\|Grid\|Land Cover\|Forest\|Natural Forest\|Secondary Forest | MAPE/MAE/MPE | Grid-level fit indicator for secondary forest
#' Fit\|Grid\|Land Cover\|Forest\|Planted Forest | MAPE/MAE/MPE | Grid-level fit indicator for planted forest
#' @md


reportFit<-function(gdx,type="MAPE", level="cell"){

  # Land types fit/error indicators

  a <- cellularFit(gdx, file=NULL, level=level, statistic=type,variable="land",dataset="LUH3")
  weight <- land(gdx, level="regglo")[,getYears(a),getNames(a)]
  mapping<-as.data.frame(getNames(a))
  colnames(mapping)<-"types"
  mapping$total<-"total"
  aAvg<-toolAggregate(a, weight=weight, rel=mapping, from="types", to="total",dim=3)

  Unit <- if (type == "MPE") "Mean percentage error %" else if (type == "MAE") "Mean absolute error ha" else if (type == "MAPE") "Mean absolute percentage error %" else type

  if(level == "cell"){
    levelName <- "Cluster"
  } else if (level == "grid") {
    levelName <- "Grid"
  }

  x <- NULL
  x <- mbind(x, setNames(a[, , "crop"], paste0("Fit|",levelName,"|Land Cover|" , reportingnames(getNames(a[, , "crop"], dim = 1)), " (",Unit,")")))
  if ("range" %in% getNames(a, dim = 1)){
    x <- mbind(x, setNames(a[, , "past"], paste0("Fit|",levelName,"|Land Cover|" , reportingnames("pastr"), " (",Unit,")")))
    x <- mbind(x, setNames(a[, , "range"], paste0("Fit|",levelName,"|Land Cover|" , reportingnames("range"), " (",Unit,")")))
  } else {
    x <- mbind(x, setNames(a[, , "past"], paste0("Fit|",levelName,"|Land Cover|" , reportingnames(getNames(a[, , "past"], dim = 1)), " (",Unit,")")))
  }
  x <- mbind(x, setNames(a[, , "urban"], paste0("Fit|",levelName,"|Land Cover|", reportingnames(getNames(a[, , "urban"], dim = 1)), " (",Unit, ")")))
  x <- mbind(x, setNames(a[, , "other"], paste0("Fit|",levelName,"|Land Cover|", reportingnames(getNames(a[, , "other"], dim = 1)), " (",Unit,")")))
  x <- mbind(x, setNames(a[, , "primforest"], paste0("Fit|",levelName,"|Land Cover|Forest|Natural Forest|", reportingnames("primforest"), " (",Unit,")")))
  x <- mbind(x, setNames(a[, , "secdforest"], paste0("Fit|",levelName,"|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), " (",Unit,")")))
  x <- mbind(x, setNames(a[, , "forestry"], paste0("Fit|",levelName,"|Land Cover|Forest|", reportingnames("forestry"), " (",Unit,")")))
  x <- mbind(x, setNames(aAvg, paste0("Fit|",levelName,"|Land Cover"," (",Unit,")")))

  # Crop types error indicators
  if (level == "cell"){
    a <- cellularFit(gdx, file=NULL, level=level, statistic=type,variable="crop",dataset="LUH3",water_aggr =TRUE)
    a[!is.finite(a)]<-0
    getNames(a)<-paste0("Fit|",levelName,"|Land Cover|Cropland|",reportingnames(getNames(a))," (",Unit,")")
    a<-a[,getYears(x),]
  } else if (level == "grid") {
    a <- NULL
  }

  # Fill magpie object

  t <- as.character(readGDX(gdx,"t"))
  out <- mbind(x,a)
  years <- intersect(t, getYears(out))
  t <- t[!t%in%years]
  out <- add_columns(out, dim=2.1, addnm =t)

  return(out)
}
